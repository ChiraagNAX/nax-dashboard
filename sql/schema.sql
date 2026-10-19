-- NAX Trader Dashboard schema (Postgres / Supabase)
-- Run once against a fresh Supabase project. Idempotent.

create extension if not exists pgcrypto;

-- ---------------------------------------------------------------------------
-- levels: static reference, one row per programme level
-- ---------------------------------------------------------------------------
create table if not exists levels (
    level_code           text primary key,          -- 'trial','L1','L2','L3'
    display_name         text not null,
    capital_usd          numeric not null,
    soft_dd_pct          numeric not null,
    hard_stop_pct        numeric not null,
    max_leverage         numeric not null,
    max_net_pct          numeric not null,
    max_gross_pct        numeric not null,
    profit_share_pct     numeric not null default 50,
    universe             text    not null default 'bybit-any'  -- 'bybit-any','top100-mc'
);

insert into levels values
    ('trial','Trial',    500,   10, 15, 4, 120, 180,  0, 'bybit-any'),
    ('L1',   'Level 1', 10000,   9, 13, 5, 120, 220, 50, 'bybit-any'),
    ('L2',   'Level 2', 25000,   8, 12, 5, 110, 200, 50, 'top100-mc'),
    ('L3',   'Level 3', 50000,   7, 11, 4, 100, 180, 50, 'top100-mc')
on conflict (level_code) do update set
    display_name=excluded.display_name,
    capital_usd=excluded.capital_usd,
    soft_dd_pct=excluded.soft_dd_pct,
    hard_stop_pct=excluded.hard_stop_pct,
    max_leverage=excluded.max_leverage,
    max_net_pct=excluded.max_net_pct,
    max_gross_pct=excluded.max_gross_pct,
    profit_share_pct=excluded.profit_share_pct,
    universe=excluded.universe;

-- ---------------------------------------------------------------------------
-- traders: one row per funded trader
-- ---------------------------------------------------------------------------
create table if not exists traders (
    trader_id            text primary key,          -- short slug, e.g. 'roja'
    display_name         text not null,
    entity_name          text,                      -- legal entity for statements
    level_code           text not null references levels(level_code),
    starting_equity_usd  numeric not null,
    window_length_days   int    not null default 14,
    window_anchor        timestamptz not null,      -- first window starts here (UTC)
    bybit_key_ref        text,                      -- lookup name only, not the key itself
    usdt_address         text,                      -- last 6 shown in UI, full for statement
    usdt_network         text,                      -- 'TRC20','ERC20','BEP20','ARBITRUM'
    holdback_pct         numeric not null default 0,
    frozen               boolean not null default false,
    created_at           timestamptz not null default now()
);

-- ---------------------------------------------------------------------------
-- windows: materialised rolling fortnights per trader
-- Boundaries are [window_start, window_end) UTC, exclusive end.
-- Populated by snapshot writer on first run and at each rollover.
-- ---------------------------------------------------------------------------
create table if not exists windows (
    window_id            bigserial primary key,
    trader_id            text not null references traders(trader_id),
    window_start         timestamptz not null,
    window_end           timestamptz not null,
    starting_equity_usd  numeric not null,          -- equity at window start (= prev end for rolls)
    index_start_level    numeric,                   -- NAX index level at window start
    closed               boolean not null default false,
    closed_at            timestamptz,
    unique (trader_id, window_start)
);
create index if not exists windows_trader_idx on windows(trader_id, window_start desc);

-- ---------------------------------------------------------------------------
-- snapshots: one row per poll per trader
-- ---------------------------------------------------------------------------
create table if not exists snapshots (
    snapshot_id          bigserial primary key,
    trader_id            text not null references traders(trader_id),
    captured_at          timestamptz not null,
    equity_usd           numeric not null,          -- raw totalEquity from Bybit
    equity_adj_usd       numeric not null,          -- transfer-adjusted (for DD/HWM)
    unrealised_pnl_usd   numeric,
    net_exposure_pct     numeric,
    gross_exposure_pct   numeric,
    max_leverage         numeric,
    position_count       int,
    hwm_usd              numeric,                   -- trailing HWM at this moment
    drawdown_pct         numeric,                   -- (hwm - equity_adj) / hwm * 100
    index_level          numeric,
    source               text not null default 'bybit'
);
create index if not exists snapshots_trader_time on snapshots(trader_id, captured_at desc);

-- ---------------------------------------------------------------------------
-- positions: per-snapshot position breakdown (for current view only; prune old)
-- ---------------------------------------------------------------------------
create table if not exists positions (
    position_id          bigserial primary key,
    snapshot_id          bigint not null references snapshots(snapshot_id) on delete cascade,
    symbol               text not null,
    side                 text not null,
    size                 numeric not null,
    entry_price          numeric,
    mark_price           numeric,
    notional_usd         numeric,
    leverage             numeric,
    unrealised_pnl_usd   numeric
);
create index if not exists positions_snap_idx on positions(snapshot_id);

-- ---------------------------------------------------------------------------
-- transfers: deposits, withdrawals, settlement sweeps
-- equity_adj_usd = equity_usd - cumulative_net_transfer_since_window_start
-- so an outbound sweep does not look like a loss.
-- ---------------------------------------------------------------------------
create table if not exists transfers (
    transfer_id          bigserial primary key,
    trader_id            text not null references traders(trader_id),
    occurred_at          timestamptz not null,
    amount_usd           numeric not null,          -- positive = deposit, negative = withdrawal
    kind                 text not null,             -- 'deposit','withdrawal','settlement','internal'
    note                 text,
    source               text not null default 'manual'   -- 'bybit-api','manual'
);
create index if not exists transfers_trader_time on transfers(trader_id, occurred_at desc);

-- ---------------------------------------------------------------------------
-- breaches: one row per breach event
-- ---------------------------------------------------------------------------
create table if not exists breaches (
    breach_id            bigserial primary key,
    trader_id            text not null references traders(trader_id),
    window_id            bigint references windows(window_id),
    occurred_at          timestamptz not null,
    category             text not null,             -- 'critical','major','soft'
    kind                 text not null,             -- 'hard_stop','max_leverage','net_exposure','gross_exposure','soft_dd','universe','unauthorised'
    observed_value       numeric,
    limit_value          numeric,
    note                 text,
    snapshot_id          bigint references snapshots(snapshot_id)
);
create index if not exists breaches_trader_time on breaches(trader_id, occurred_at desc);

-- ---------------------------------------------------------------------------
-- benchmark: NAX Crypto Index returns (from fetch-benchmarks.ps1 for now)
-- Per-trader per-window return; writer updates on each refresh.
-- ---------------------------------------------------------------------------
create table if not exists benchmark_returns (
    trader_id            text not null references traders(trader_id),
    window_id            bigint not null references windows(window_id),
    captured_at          timestamptz not null,
    return_pct           numeric not null,
    primary key (trader_id, window_id, captured_at)
);

-- ---------------------------------------------------------------------------
-- payouts: one row per closed window; computed by writer at window close
-- ---------------------------------------------------------------------------
create table if not exists payouts (
    payout_id            bigserial primary key,
    trader_id            text not null references traders(trader_id),
    window_id            bigint not null unique references windows(window_id),
    starting_equity_usd  numeric not null,
    ending_equity_usd    numeric not null,
    net_profit_usd       numeric not null,
    index_return_pct     numeric,
    benchmark_equiv_usd  numeric not null,          -- floored at 0
    outperformance_usd   numeric not null,
    profit_share_pct     numeric not null,
    eligible_payout_usd  numeric not null,
    holdback_usd         numeric not null default 0,
    payable_now_usd      numeric not null,          -- eligible - holdback
    holdback_release_at  timestamptz,
    computed_at          timestamptz not null default now()
);

-- ---------------------------------------------------------------------------
-- payout_ledger: NAX staff updates these; dashboard shows status to trader
-- ---------------------------------------------------------------------------
create table if not exists payout_ledger (
    ledger_id            bigserial primary key,
    payout_id            bigint not null unique references payouts(payout_id),
    status               text not null default 'settled',   -- 'settled','invoiced','paid'
    invoiced_at          timestamptz,
    invoice_ref          text,
    paid_at              timestamptz,
    paid_amount_usd      numeric,
    paid_tx_id           text,
    notes                text,
    updated_at           timestamptz not null default now(),
    updated_by           text
);

-- ---------------------------------------------------------------------------
-- users: auth for Shiny app (shinymanager-compatible)
-- ---------------------------------------------------------------------------
create table if not exists users (
    user_id              text primary key,          -- login name
    password_hash        text not null,             -- bcrypt
    role                 text not null default 'trader',   -- 'trader','admin'
    trader_id            text references traders(trader_id),  -- null for admins
    created_at           timestamptz not null default now(),
    last_login_at        timestamptz
);

-- ---------------------------------------------------------------------------
-- admin_audit: every admin mutation
-- ---------------------------------------------------------------------------
create table if not exists admin_audit (
    audit_id             bigserial primary key,
    occurred_at          timestamptz not null default now(),
    actor                text not null,
    action               text not null,
    target_table         text,
    target_id            text,
    before_json          jsonb,
    after_json           jsonb,
    note                 text
);

-- ---------------------------------------------------------------------------
-- Seed: Roja (Level 1), holdback 0 per Level 1 notice, window 2026-09-30 00:00 UTC
-- ---------------------------------------------------------------------------
insert into traders (trader_id, display_name, entity_name, level_code, starting_equity_usd,
                     window_length_days, window_anchor, bybit_key_ref, holdback_pct, frozen)
values ('roja', 'Roja Shakour', 'Shakour Investments', 'L1', 10000,
        14, '2026-09-30T00:00:00Z', 'roja_bybit_ro', 0, false)
on conflict (trader_id) do nothing;
