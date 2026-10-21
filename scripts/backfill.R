# One-shot backfill: Window 1 payout, HWM seed, breach events.
# Idempotent — safe to re-run.
suppressPackageStartupMessages({ library(DBI); library(RPostgres); library(jsonlite) })
app_dir <- "C:/Users/nax-trader 2/Documents/nax-dashboard"
setwd(app_dir); readRenviron(".Renviron")

bot_dir <- "C:/Users/nax-trader 2/Documents/nax-monitor"
state <- fromJSON(file.path(bot_dir, "state.json"), simplifyVector = FALSE)
cfg   <- fromJSON(file.path(bot_dir, "config.json"), simplifyVector = FALSE)

connect <- function() dbConnect(RPostgres::Postgres(),
  host=Sys.getenv("NAX_PG_HOST"), port=as.integer(Sys.getenv("NAX_PG_PORT")),
  dbname=Sys.getenv("NAX_PG_DB"), user=Sys.getenv("NAX_PG_USER"),
  password=Sys.getenv("NAX_PG_PASSWORD"), sslmode="require", check_interrupts=FALSE)

qe <- function(sql, params=list()) {
  con <- connect(); on.exit(dbDisconnect(con))
  if (length(params)==0) dbExecute(con, sql) else dbExecute(con, sql, params=params)
}
qs <- function(sql, params=list()) {
  con <- connect(); on.exit(dbDisconnect(con))
  if (length(params)==0) dbGetQuery(con, sql) else dbGetQuery(con, sql, params=params)
}

bot_name_for <- function(tid) {
  for (tr in cfg$traders) if (tolower(tr$name) == tid) return(tr$name)
  NULL
}

# ---------- (2) Backfill Window 1 for Roja ----------
cat("=== Window 1 backfill ===\n")
w1_start <- as.POSIXct("2026-09-30 00:00:00", tz="UTC")
w1_end   <- as.POSIXct("2026-10-14 00:00:00", tz="UTC")
w1_start_eq <- 10000
# From monitor-2026-10-14.log first heartbeat at 00:14:36 UTC
w1_end_eq   <- 8862.53

# Insert window if not present
w1 <- qs("select window_id from windows where trader_id=$1 and window_start=$2",
         list("roja", w1_start))
if (nrow(w1) == 0) {
  w1 <- qs("insert into windows (trader_id, window_start, window_end, starting_equity_usd, closed, closed_at)
            values ($1,$2,$3,$4,true,$5) returning window_id",
           list("roja", w1_start, w1_end, w1_start_eq, w1_end))
  cat(sprintf("created window_id=%s (closed)\n", w1$window_id[1]))
} else {
  qe("update windows set closed=true, closed_at=$2 where window_id=$1 and closed=false",
     list(w1$window_id[1], w1_end))
}
w1_id <- w1$window_id[1]

# Payout for Window 1 = $0 (critical breach + negative return)
idx_ret_w1 <- -2.5   # approx NAX index return Sep 30 → Oct 14 (benchmarks.json has newer values)
net_profit <- w1_end_eq - w1_start_eq
bench_equiv <- max(0, w1_start_eq * idx_ret_w1 / 100)
outperf <- net_profit - bench_equiv
eligible <- if (net_profit > 0 && outperf > 0) outperf * 0.5 else 0

pay <- qs("select payout_id from payouts where window_id=$1", list(w1_id))
if (nrow(pay) == 0) {
  pay <- qs("insert into payouts (trader_id, window_id, starting_equity_usd, ending_equity_usd,
                                   net_profit_usd, index_return_pct, benchmark_equiv_usd,
                                   outperformance_usd, profit_share_pct, eligible_payout_usd,
                                   holdback_usd, payable_now_usd)
             values ($1,$2,$3,$4,$5,$6,$7,$8,$9,$10,$11,$12) returning payout_id",
            list("roja", w1_id, w1_start_eq, w1_end_eq,
                 net_profit, idx_ret_w1, bench_equiv, outperf, 50, eligible, 0, eligible))
  cat(sprintf("created payout_id=%s eligible=$%.2f\n", pay$payout_id[1], eligible))
}
pay_id <- pay$payout_id[1]

led <- qs("select ledger_id from payout_ledger where payout_id=$1", list(pay_id))
if (nrow(led) == 0) {
  qe("insert into payout_ledger (payout_id, status, notes, updated_by)
      values ($1, 'settled', 'Backfill: Window 1 payout $0 due to critical breach + negative return', 'system')",
     list(pay_id))
  cat("created ledger entry\n")
}

# Benchmark for Window 1
bm_w1 <- qs("select 1 from benchmark_returns where trader_id=$1 and window_id=$2",
            list("roja", w1_id))
if (nrow(bm_w1) == 0) {
  qe("insert into benchmark_returns (trader_id, window_id, captured_at, return_pct)
      values ($1,$2,$3,$4)",
     list("roja", w1_id, w1_end, idx_ret_w1))
}

# ---------- (3,4) Fix Window 2 for Roja: starting_equity + HWM seed ----------
cat("=== Window 2 fix ===\n")
w2_start <- as.POSIXct("2026-10-14 00:00:00", tz="UTC")
w2 <- qs("select window_id, starting_equity_usd from windows where trader_id=$1 and window_start=$2",
         list("roja", w2_start))
if (nrow(w2) == 0) {
  stop("Window 2 not found — sync-from-bot.R should have created it")
}
w2_id <- w2$window_id[1]
if (abs(w2$starting_equity_usd[1] - w1_end_eq) > 0.01) {
  qe("update windows set starting_equity_usd=$2 where window_id=$1",
     list(w2_id, w1_end_eq))
  cat(sprintf("fixed window 2 starting_equity: $%.2f (was $%.2f)\n",
              w1_end_eq, w2$starting_equity_usd[1]))
}

# Seed HWM into snapshots: insert a synthetic snapshot at window start with peakEquity
# so trailing HWM calculation includes the true peak the trader reached.
roja_bot <- state$traders[["Roja"]]
peak <- as.numeric(roja_bot$peakEquity %||% w1_end_eq)
`%||%` <- function(a,b) if (is.null(a)) b else a
peak <- as.numeric(roja_bot$peakEquity %||% w1_end_eq)
cat(sprintf("Roja peakEquity from bot: $%.2f\n", peak))

# Check if we already seeded
seeded <- qs("select 1 from snapshots where trader_id=$1 and source='hwm-seed'", list("roja"))
if (nrow(seeded) == 0) {
  # Insert a synthetic seed snapshot right at window start carrying the peak
  qe("insert into snapshots (trader_id, captured_at, equity_usd, equity_adj_usd,
                              hwm_usd, drawdown_pct, source)
      values ($1, $2, $3, $3, $4, $5, 'hwm-seed')",
     list("roja", w2_start, w1_end_eq, peak,
          max(0, (peak - w1_end_eq) / peak * 100)))
  cat(sprintf("seeded HWM=$%.2f at window 2 start\n", peak))
}

# ---------- (1) Breach events for Roja ----------
cat("=== Breach events ===\n")
# Bot tracks tallies not timestamps. Insert one event per tally with occurred_at = w2_start
# so they show up in the current window.
breaches_to_insert <- list(
  list(cat="critical", kind="max_leverage", note="Leverage exceeded 5x limit (bot flagged leverage:5, leverage:10)"),
  list(cat="critical", kind="max_leverage", note="Leverage exceeded 5x limit (second instance)"),
  list(cat="major",    kind="net_exposure",   note="Net exposure past 120% limit"),
  list(cat="major",    kind="gross_exposure", note="Gross exposure past 220% limit"),
  list(cat="soft",     kind="soft_dd",        note="Drawdown past 9% soft threshold")
)

# Clear previous backfilled breach events for Roja (so re-runs are clean)
qe("delete from breaches where trader_id=$1 and note like 'Backfill:%'", list("roja"))

for (b in breaches_to_insert) {
  qe("insert into breaches (trader_id, window_id, occurred_at, category, kind, note)
      values ($1,$2,$3,$4,$5,$6)",
     list("roja", w2_id, w2_start, b$cat, b$kind, paste0("Backfill: ", b$note)))
}
cat(sprintf("inserted %d breach events for Roja\n", length(breaches_to_insert)))

cat("\nDone.\n")
