# Reads the nax-monitor bot's state.json and writes snapshots/breaches/windows
# into Supabase. Run on a schedule (every 60s) for live data.
#
# Usage: Rscript scripts/sync-from-bot.R
# Idempotent: safe to run repeatedly.

suppressPackageStartupMessages({
  library(DBI); library(RPostgres); library(jsonlite)
})
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a
app_dir <- "C:/Users/nax-trader 2/Documents/nax-dashboard"
setwd(app_dir)
readRenviron(".Renviron")

connect <- function() dbConnect(RPostgres::Postgres(),
  host=Sys.getenv("NAX_PG_HOST"),
  port=as.integer(Sys.getenv("NAX_PG_PORT")),
  dbname=Sys.getenv("NAX_PG_DB"),
  user=Sys.getenv("NAX_PG_USER"),
  password=Sys.getenv("NAX_PG_PASSWORD"),
  sslmode="require",
  check_interrupts=FALSE)

qe1 <- function(sql, params = list()) {
  con <- connect(); on.exit(dbDisconnect(con))
  if (length(params) == 0) dbExecute(con, sql) else dbExecute(con, sql, params = params)
}
qs1 <- function(sql, params = list()) {
  con <- connect(); on.exit(dbDisconnect(con))
  if (length(params) == 0) dbGetQuery(con, sql) else dbGetQuery(con, sql, params = params)
}

bot_dir <- "C:/Users/nax-trader 2/Documents/nax-monitor"
cfg <- fromJSON(file.path(bot_dir, "config.json"), simplifyVector = FALSE)
state <- fromJSON(file.path(bot_dir, "state.json"), simplifyVector = FALSE)
bench_path <- file.path(bot_dir, "benchmarks.json")
bench <- if (file.exists(bench_path)) fromJSON(bench_path, simplifyVector = FALSE) else list(returnsPct = list())

# For each trader in the DB, pull their matching bot state
traders <- qs1("select trader_id, display_name, starting_equity_usd, window_length_days, window_anchor from traders")

for (i in seq_len(nrow(traders))) {
  tid <- traders$trader_id[i]
  bot_name <- NULL
  # Match trader_id to bot config trader name (lowercase slug)
  for (tr in cfg$traders) {
    if (tolower(tr$name) == tid) { bot_name <- tr$name; break }
  }
  if (is.null(bot_name) || is.null(state$traders[[bot_name]])) {
    cat(sprintf("skip %s (not in bot state)\n", tid)); next
  }

  bot_t <- state$traders[[bot_name]]
  snap  <- bot_t$lastSnapshot
  if (is.null(snap)) { cat(sprintf("skip %s (no snapshot)\n", tid)); next }

  # --- Window: compute current from anchor + length ---
  anchor <- as.POSIXct(traders$window_anchor[i], tz = "UTC")
  len    <- traders$window_length_days[i]
  now    <- Sys.time()
  elapsed_days <- as.numeric(difftime(now, anchor, units = "days"))
  if (elapsed_days < 0) { cat(sprintf("skip %s (pre-anchor)\n", tid)); next }
  n <- floor(elapsed_days / len)
  w_start <- anchor + n * len * 86400
  w_end   <- w_start + len * 86400
  start_eq <- traders$starting_equity_usd[i]  # TODO: use prior window close for later windows

  # Ensure window row
  wid <- qs1("select window_id from windows where trader_id=$1 and window_start=$2",
             list(tid, w_start))
  if (nrow(wid) == 0) {
    wid <- qs1("insert into windows (trader_id, window_start, window_end, starting_equity_usd)
                values ($1,$2,$3,$4) returning window_id",
               list(tid, w_start, w_end, start_eq))
    cat(sprintf("created window %s for %s\n", wid$window_id[1], tid))
  }
  wid <- wid$window_id[1]

  # --- Snapshot ---
  equity <- as.numeric(snap$equity %||% 0)
  # Transfer-adjusted = raw for now (no transfers tracked yet)
  eq_adj <- equity
  # Trailing HWM: take max of (prior max, current)
  prev_hwm <- qs1("select coalesce(max(equity_adj_usd), $2::numeric) as hwm from snapshots
                   where trader_id=$1 and captured_at >= $3",
                  list(tid, start_eq, w_start))$hwm[1]
  hwm <- max(as.numeric(prev_hwm), eq_adj)
  dd  <- if (hwm > 0) max(0, (hwm - eq_adj) / hwm * 100) else 0

  qe1("insert into snapshots (trader_id, captured_at, equity_usd, equity_adj_usd,
                              net_exposure_pct, gross_exposure_pct, max_leverage,
                              position_count, hwm_usd, drawdown_pct, source)
       values ($1, now(), $2, $3, $4, $5, $6, $7, $8, $9, 'bot-sync')",
      list(tid, equity, eq_adj,
           as.numeric(snap$netPct %||% 0),
           as.numeric(snap$grossPct %||% 0),
           as.numeric(snap$maxLev %||% 0),
           as.integer(snap$positions %||% 0),
           hwm, dd))

  # --- Benchmark ---
  br <- bench$returnsPct[[bot_name]]
  if (!is.null(br)) {
    qe1("insert into benchmark_returns (trader_id, window_id, captured_at, return_pct)
         values ($1,$2, now(), $3)
         on conflict (trader_id, window_id, captured_at) do nothing",
        list(tid, wid, as.numeric(br)))
  }

  cat(sprintf("synced %s: eq=$%.2f dd=%.2f%% idx=%s%%\n",
              tid, equity, dd, if (is.null(br)) "n/a" else format(br)))
}

cat("done.\n")
