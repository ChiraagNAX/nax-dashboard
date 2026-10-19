# DB helpers. Single connection pool via pool package.
# Reads NAX_PG_* env vars. Safe for shinyapps.io (no local files).

suppressPackageStartupMessages({
  library(DBI)
  library(RPostgres)
  library(pool)
})

.nax_pool <- NULL

nax_pool <- function() {
  if (!is.null(.nax_pool) && pool::dbIsValid(.nax_pool)) return(.nax_pool)
  .nax_pool <<- pool::dbPool(
    drv      = RPostgres::Postgres(),
    host     = Sys.getenv("NAX_PG_HOST"),
    port     = as.integer(Sys.getenv("NAX_PG_PORT", "5432")),
    dbname   = Sys.getenv("NAX_PG_DB", "postgres"),
    user     = Sys.getenv("NAX_PG_USER"),
    password = Sys.getenv("NAX_PG_PASSWORD"),
    sslmode  = "require",
    check_interrupts = FALSE,
    bigint   = "integer",
    minSize  = 1,
    maxSize  = 4,
    validate = "select 1"
  )
  .nax_pool
}

q <- function(sql, ...) {
  params <- list(...)
  if (length(params) == 0) return(DBI::dbGetQuery(nax_pool(), sql))
  DBI::dbGetQuery(nax_pool(), sql, params = unname(params))
}

qe <- function(sql, ...) {
  params <- list(...)
  if (length(params) == 0) return(DBI::dbExecute(nax_pool(), sql))
  DBI::dbExecute(nax_pool(), sql, params = unname(params))
}

# -------- domain reads --------

get_trader <- function(trader_id) {
  q("select t.*, l.display_name as level_name, l.capital_usd, l.soft_dd_pct, l.hard_stop_pct,
            l.max_leverage, l.max_net_pct, l.max_gross_pct, l.profit_share_pct, l.universe
     from traders t join levels l on t.level_code = l.level_code
     where t.trader_id = $1", trader_id)
}

get_current_window <- function(trader_id, now_utc = Sys.time()) {
  q("select * from windows
     where trader_id = $1 and window_start <= $2 and window_end > $2
     order by window_start desc limit 1",
    trader_id, now_utc)
}

get_latest_snapshot <- function(trader_id) {
  q("select * from snapshots where trader_id = $1
     order by captured_at desc limit 1", trader_id)
}

get_equity_series <- function(trader_id, since) {
  q("select captured_at, equity_adj_usd, hwm_usd
     from snapshots where trader_id = $1 and captured_at >= $2
     order by captured_at", trader_id, since)
}

get_positions_for_snapshot <- function(snapshot_id) {
  q("select symbol, side, size, entry_price, mark_price, notional_usd, leverage, unrealised_pnl_usd
     from positions where snapshot_id = $1 order by abs(notional_usd) desc", snapshot_id)
}

get_breaches_in_window <- function(trader_id, window_id) {
  q("select * from breaches where trader_id = $1 and window_id = $2
     order by occurred_at desc", trader_id, window_id)
}

get_latest_benchmark <- function(trader_id, window_id) {
  q("select return_pct, captured_at from benchmark_returns
     where trader_id = $1 and window_id = $2
     order by captured_at desc limit 1", trader_id, window_id)
}

get_payouts <- function(trader_id) {
  q("select p.*, w.window_start, w.window_end,
            coalesce(l.status, 'settled') as ledger_status,
            l.invoiced_at, l.paid_at, l.paid_tx_id
     from payouts p
     join windows w on p.window_id = w.window_id
     left join payout_ledger l on l.payout_id = p.payout_id
     where p.trader_id = $1
     order by w.window_end desc", trader_id)
}

# -------- auth --------

get_user <- function(user_id) {
  q("select * from users where user_id = $1", user_id)
}

touch_login <- function(user_id) {
  qe("update users set last_login_at = now() where user_id = $1", user_id)
}
