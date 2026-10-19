#Requires -Version 5.1
# NAX snapshot writer. Runs alongside the existing nax-monitor.
# Polls each active trader every $PollSeconds, computes transfer-adjusted
# equity + trailing HWM, writes a row to Postgres.
#
# Reuses the bot's Bybit calls (same signing, same math) by sourcing them.
# Does NOT touch Telegram or breach alerting — that stays in monitor.ps1.
#
# Config:
#   - config.json in nax-monitor dir supplies trader list and keys.
#   - secrets.local.ps1 (dot-sourced) supplies NAX_PG_* env vars.

[CmdletBinding()]
param(
    [string]$BotDir      = (Join-Path (Split-Path -Parent $PSScriptRoot) '..\nax-monitor' | Resolve-Path -ErrorAction SilentlyContinue),
    [string]$ConfigPath,
    [int]   $PollSeconds = 60,
    [string]$SecretsFile = (Join-Path (Split-Path -Parent $PSScriptRoot) 'config\secrets.local.ps1')
)

$ErrorActionPreference = 'Stop'
[Net.ServicePointManager]::SecurityProtocol = [Net.SecurityProtocolType]::Tls12

if (-not $BotDir -or -not (Test-Path $BotDir)) {
    $BotDir = 'C:\Users\nax-trader 2\Documents\nax-monitor'
}
if (-not $ConfigPath) { $ConfigPath = Join-Path $BotDir 'config.json' }
if (Test-Path $SecretsFile) { . $SecretsFile } else { Write-Warning "Secrets file not found: $SecretsFile" }

$scriptDir = $PSScriptRoot
$logDir = Join-Path (Split-Path -Parent $scriptDir) 'logs'
if (-not (Test-Path $logDir)) { New-Item -ItemType Directory -Path $logDir -Force | Out-Null }

function Write-Log {
    param([string]$Msg, [string]$Level = 'INFO')
    $line = "[{0}] [{1}] {2}" -f (Get-Date).ToString('yyyy-MM-dd HH:mm:ss'), $Level, $Msg
    try { Write-Host $line } catch {}
    $f = Join-Path $logDir ("writer-{0}.log" -f (Get-Date -Format 'yyyy-MM-dd'))
    try { Add-Content -Path $f -Value $line -Encoding utf8 } catch {}
}

# -----------------------------------------------------------------------------
# Bybit calls — identical signing to monitor.ps1 so results match
# -----------------------------------------------------------------------------
function Invoke-BybitGet {
    param([string]$ApiKey, [string]$ApiSecret, [string]$Path, [hashtable]$Query = @{})
    $recv = '20000'
    $ts   = [string][DateTimeOffset]::UtcNow.ToUnixTimeMilliseconds()
    $qs   = ($Query.GetEnumerator() | Sort-Object Name | ForEach-Object { "$($_.Name)=$($_.Value)" }) -join '&'
    $toSign = "$ts$ApiKey$recv$qs"
    $hmac = New-Object System.Security.Cryptography.HMACSHA256
    $hmac.Key = [Text.Encoding]::UTF8.GetBytes($ApiSecret)
    $sig = ($hmac.ComputeHash([Text.Encoding]::UTF8.GetBytes($toSign)) | ForEach-Object { $_.ToString('x2') }) -join ''
    $url = "https://api.bybit.com$Path"
    if ($qs) { $url = "$url`?$qs" }
    $headers = @{
        'X-BAPI-API-KEY'     = $ApiKey
        'X-BAPI-TIMESTAMP'   = $ts
        'X-BAPI-RECV-WINDOW' = $recv
        'X-BAPI-SIGN'        = $sig
    }
    Invoke-RestMethod -Method Get -Uri $url -Headers $headers -TimeoutSec 20
}

function Get-AccountSnapshot {
    param($Trader)
    $wallet = Invoke-BybitGet -ApiKey $Trader.apiKey -ApiSecret $Trader.apiSecret -Path '/v5/account/wallet-balance' -Query @{ accountType = 'UNIFIED' }
    if ($wallet.retCode -ne 0) { throw "wallet-balance retCode=$($wallet.retCode) msg=$($wallet.retMsg)" }
    $acct = $wallet.result.list | Select-Object -First 1
    $equity = [double]$acct.totalEquity

    $positions = Invoke-BybitGet -ApiKey $Trader.apiKey -ApiSecret $Trader.apiSecret -Path '/v5/position/list' -Query @{ category = 'linear'; settleCoin = 'USDT' }
    if ($positions.retCode -ne 0) { throw "position/list retCode=$($positions.retCode) msg=$($positions.retMsg)" }
    $rows = @($positions.result.list | Where-Object { [double]$_.size -gt 0 })

    $net = 0.0; $gross = 0.0; $maxLev = 0.0; $upl = 0.0
    $posList = @()
    foreach ($p in $rows) {
        $size  = [double]$p.size
        $mark  = if ($p.markPrice) { [double]$p.markPrice } else { [double]$p.avgPrice }
        $not   = $size * $mark
        $signed = if ($p.side -eq 'Buy') { $not } else { -$not }
        $net   += $signed
        $gross += [Math]::Abs($not)
        $lev = if ($p.leverage) { [double]$p.leverage } else { 0 }
        if ($lev -gt $maxLev) { $maxLev = $lev }
        $u = if ($p.unrealisedPnl) { [double]$p.unrealisedPnl } else { 0 }
        $upl += $u
        $posList += [pscustomobject]@{
            symbol=$p.symbol; side=$p.side; size=$size
            entry=[double]$p.avgPrice; mark=$mark; notional=$not
            lev=$lev; upl=$u
        }
    }
    $netPct   = if ($equity -gt 0) { $net / $equity * 100 } else { 0 }
    $grossPct = if ($equity -gt 0) { $gross / $equity * 100 } else { 0 }

    [pscustomobject]@{
        Equity=$equity; NetPct=$netPct; GrossPct=$grossPct
        MaxLev=$maxLev; Positions=$posList; UPL=$upl
    }
}

# -----------------------------------------------------------------------------
# Postgres via Npgsql (bundled with .NET assemblies). Fallback: pg REST API.
# For simplicity we use the Supabase REST API — no driver install needed.
# NAX_PG_* env vars still describe the DB; we also need NAX_SB_URL + NAX_SB_KEY
# for the REST path. If those are missing, we try the npgsql direct connection.
# -----------------------------------------------------------------------------

function Get-NpgsqlConn {
    if ($script:PgConn -and $script:PgConn.State -eq 'Open') { return $script:PgConn }
    # Try to load Npgsql from the user's NuGet or dotnet cache
    $candidates = @(
        "$env:USERPROFILE\.nuget\packages\npgsql\*\lib\net6.0\Npgsql.dll",
        "$env:USERPROFILE\.nuget\packages\npgsql\*\lib\net8.0\Npgsql.dll"
    )
    $dll = $null
    foreach ($pat in $candidates) {
        $m = Get-ChildItem $pat -ErrorAction SilentlyContinue | Sort-Object FullName -Descending | Select-Object -First 1
        if ($m) { $dll = $m.FullName; break }
    }
    if (-not $dll) {
        throw "Npgsql.dll not found. Install with: dotnet add package Npgsql, or run: nuget install Npgsql -OutputDirectory `"$env:USERPROFILE\.nuget\packages`""
    }
    Add-Type -Path $dll
    $cs = "Host=$($env:NAX_PG_HOST);Port=$($env:NAX_PG_PORT);Database=$($env:NAX_PG_DB);Username=$($env:NAX_PG_USER);Password=$($env:NAX_PG_PASSWORD);SSL Mode=Require;Trust Server Certificate=true"
    $script:PgConn = New-Object Npgsql.NpgsqlConnection($cs)
    $script:PgConn.Open()
    return $script:PgConn
}

function Invoke-PgNonQuery {
    param([string]$Sql, [hashtable]$Params = @{})
    $c = Get-NpgsqlConn
    $cmd = $c.CreateCommand()
    $cmd.CommandText = $Sql
    foreach ($k in $Params.Keys) {
        $p = $cmd.CreateParameter(); $p.ParameterName = $k; $p.Value = if ($null -eq $Params[$k]) { [DBNull]::Value } else { $Params[$k] }
        [void]$cmd.Parameters.Add($p)
    }
    [void]$cmd.ExecuteNonQuery()
}

function Invoke-PgScalar {
    param([string]$Sql, [hashtable]$Params = @{})
    $c = Get-NpgsqlConn
    $cmd = $c.CreateCommand()
    $cmd.CommandText = $Sql
    foreach ($k in $Params.Keys) {
        $p = $cmd.CreateParameter(); $p.ParameterName = $k; $p.Value = if ($null -eq $Params[$k]) { [DBNull]::Value } else { $Params[$k] }
        [void]$cmd.Parameters.Add($p)
    }
    $cmd.ExecuteScalar()
}

# -----------------------------------------------------------------------------
# Window + HWM state (per trader) — queried from DB each poll
# -----------------------------------------------------------------------------

function Get-OrCreate-Window {
    param([string]$TraderId, [datetime]$Anchor, [int]$LengthDays, [datetime]$Now, [double]$CurrentEquity)
    $elapsed = ($Now - $Anchor).TotalDays
    if ($elapsed -lt 0) { return $null }
    $n = [Math]::Floor($elapsed / $LengthDays)
    $wStart = $Anchor.AddDays($n * $LengthDays)
    $wEnd   = $wStart.AddDays($LengthDays)

    $existingId = Invoke-PgScalar "select window_id from windows where trader_id=@t and window_start=@s" @{ t=$TraderId; s=$wStart }
    if ($null -ne $existingId -and $existingId -isnot [DBNull]) { return @{ id=[long]$existingId; start=$wStart; end=$wEnd } }

    # Starting equity for new window = ending equity of previous window if exists, else current equity
    $prevEnd = Invoke-PgScalar "select ending_equity_usd from payouts p join windows w on p.window_id=w.window_id where w.trader_id=@t order by w.window_end desc limit 1" @{ t=$TraderId }
    $startEq = if ($null -ne $prevEnd -and $prevEnd -isnot [DBNull]) { [double]$prevEnd } else { [double]$CurrentEquity }

    $newId = Invoke-PgScalar @"
insert into windows (trader_id, window_start, window_end, starting_equity_usd)
values (@t, @s, @e, @se) returning window_id
"@ @{ t=$TraderId; s=$wStart; e=$wEnd; se=$startEq }
    Write-Log "$TraderId created window $wStart -> $wEnd (startEq=$startEq)"
    return @{ id=[long]$newId; start=$wStart; end=$wEnd }
}

function Get-TrailingHwm {
    param([string]$TraderId, [datetime]$WindowStart)
    $v = Invoke-PgScalar "select coalesce(max(equity_adj_usd),0) from snapshots where trader_id=@t and captured_at>=@s" @{ t=$TraderId; s=$WindowStart }
    if ($null -eq $v -or $v -is [DBNull]) { return 0.0 }
    [double]$v
}

function Get-NetTransfers {
    param([string]$TraderId, [datetime]$Since)
    $v = Invoke-PgScalar "select coalesce(sum(amount_usd),0) from transfers where trader_id=@t and occurred_at>=@s" @{ t=$TraderId; s=$Since }
    if ($null -eq $v -or $v -is [DBNull]) { return 0.0 }
    [double]$v
}

function Write-Snapshot {
    param([string]$TraderId, $Snap, [datetime]$WindowStart, [double]$EquityAdj, [double]$Hwm, [double]$Dd)
    $sql = @"
insert into snapshots (trader_id, captured_at, equity_usd, equity_adj_usd, unrealised_pnl_usd,
                       net_exposure_pct, gross_exposure_pct, max_leverage, position_count,
                       hwm_usd, drawdown_pct)
values (@t, now(), @eq, @eqa, @upl, @net, @gro, @lev, @pc, @hwm, @dd)
returning snapshot_id
"@
    $id = Invoke-PgScalar $sql @{
        t=$TraderId; eq=$Snap.Equity; eqa=$EquityAdj; upl=$Snap.UPL
        net=$Snap.NetPct; gro=$Snap.GrossPct; lev=$Snap.MaxLev
        pc=$Snap.Positions.Count; hwm=$Hwm; dd=$Dd
    }
    [long]$id
}

function Write-Positions {
    param([long]$SnapshotId, $Positions)
    foreach ($p in $Positions) {
        Invoke-PgNonQuery @"
insert into positions (snapshot_id, symbol, side, size, entry_price, mark_price, notional_usd, leverage, unrealised_pnl_usd)
values (@sid, @sym, @side, @sz, @ent, @mk, @not, @lev, @upl)
"@ @{ sid=$SnapshotId; sym=$p.symbol; side=$p.side; sz=$p.size; ent=$p.entry; mk=$p.mark; not=$p.notional; lev=$p.lev; upl=$p.upl }
    }
}

# -----------------------------------------------------------------------------
# Main loop
# -----------------------------------------------------------------------------

$cfg = Get-Content $ConfigPath -Raw | ConvertFrom-Json
Write-Log "writer starting, traders in config: $($cfg.traders.Count), poll=${PollSeconds}s"

while ($true) {
    foreach ($tr in $cfg.traders) {
        if ($tr.isSynthetic -or $tr.frozen -or -not $tr.apiKey) { continue }

        # Expect trader_id lower-case slug of name
        $tid = $tr.name.ToLower()

        # Confirm trader exists in DB; if not, skip (admin must add row first)
        $exists = Invoke-PgScalar "select 1 from traders where trader_id=@t" @{ t=$tid }
        if ($null -eq $exists -or $exists -is [DBNull]) {
            Write-Log "$tid not in traders table; skipping" 'WARN'
            continue
        }

        $row = Invoke-PgScalar "select window_anchor from traders where trader_id=@t" @{ t=$tid }
        $anchor = [datetime]::Parse($row).ToUniversalTime()
        $lenDays = [int](Invoke-PgScalar "select window_length_days from traders where trader_id=@t" @{ t=$tid })

        try {
            $snap = Get-AccountSnapshot -Trader $tr
        } catch {
            Write-Log "$tid snapshot failed: $_" 'ERROR'
            continue
        }

        $now = [datetime]::UtcNow
        $win = Get-OrCreate-Window -TraderId $tid -Anchor $anchor -LengthDays $lenDays -Now $now -CurrentEquity $snap.Equity
        if ($null -eq $win) { continue }

        $netXfer = Get-NetTransfers -TraderId $tid -Since $win.start
        $eqAdj = $snap.Equity - $netXfer
        $prevHwm = Get-TrailingHwm -TraderId $tid -WindowStart $win.start
        $hwm = [Math]::Max($prevHwm, $eqAdj)
        $dd = if ($hwm -gt 0) { [Math]::Max(0, ($hwm - $eqAdj) / $hwm * 100) } else { 0 }

        try {
            $sid = Write-Snapshot -TraderId $tid -Snap $snap -WindowStart $win.start -EquityAdj $eqAdj -Hwm $hwm -Dd $dd
            if ($snap.Positions.Count -gt 0) { Write-Positions -SnapshotId $sid -Positions $snap.Positions }
            Write-Log "$tid wrote snapshot eq=$($snap.Equity) adj=$eqAdj hwm=$hwm dd=$([Math]::Round($dd,2))%"
        } catch {
            Write-Log "$tid DB write failed: $_" 'ERROR'
            # Reset connection so next loop retries fresh
            try { $script:PgConn.Close() } catch {}
            $script:PgConn = $null
        }
    }
    Start-Sleep -Seconds $PollSeconds
}
