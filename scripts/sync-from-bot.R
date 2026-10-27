# Syncs the nax-monitor bot's state into Supabase, and pulls open positions and closed trades
# from Bybit using the trader's READ-ONLY key (read from the bot's config.json, never stored
# in the repo and never sent to the browser). Scheduled every minute. Idempotent.
#
# Usage: Rscript scripts/sync-from-bot.R

suppressPackageStartupMessages({
  library(DBI); library(RPostgres); library(jsonlite); library(httr2); library(digest)
})
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || (length(a) == 1 && is.na(a))) b else a
setwd("C:/Users/nax-trader 2/Documents/nax-dashboard")
readRenviron(".Renviron")

connect <- function() dbConnect(RPostgres::Postgres(),
  host=Sys.getenv("NAX_PG_HOST"), port=as.integer(Sys.getenv("NAX_PG_PORT")),
  dbname=Sys.getenv("NAX_PG_DB"), user=Sys.getenv("NAX_PG_USER"),
  password=Sys.getenv("NAX_PG_PASSWORD"), sslmode="require", check_interrupts=FALSE)

qe1 <- function(sql, params = list()) {
  con <- connect(); on.exit(dbDisconnect(con))
  if (length(params) == 0) dbExecute(con, sql) else dbExecute(con, sql, params = params)
}
qs1 <- function(sql, params = list()) {
  con <- connect(); on.exit(dbDisconnect(con))
  if (length(params) == 0) dbGetQuery(con, sql) else dbGetQuery(con, sql, params = params)
}

# Bot timestamps look like 2026-10-22T10:50:09.9642015+11:00
parse_iso <- function(x) {
  if (is.null(x) || is.na(x)) return(as.POSIXct(NA))
  x <- sub("\\.\\d+", "", x)
  x <- sub("([+-]\\d{2}):(\\d{2})$", "\\1\\2", x)
  as.POSIXct(strptime(x, "%Y-%m-%dT%H:%M:%S%z", tz = "UTC"))
}

# ---- Bybit V5 signed GET (read-only endpoints only) ----
bybit_get <- function(key, secret, path, query) {
  query <- query[order(names(query))]
  qs  <- paste(paste0(names(query), "=",
                      vapply(query, function(v) utils::URLencode(as.character(v), reserved = TRUE), "")),
               collapse = "&")
  ts   <- format(round(as.numeric(Sys.time()) * 1000), scientific = FALSE)
  recv <- "20000"
  sig  <- digest::hmac(secret, paste0(ts, key, recv, qs), "sha256")
  resp <- httr2::request(paste0("https://api.bybit.com", path, "?", qs)) |>
    httr2::req_headers(`X-BAPI-API-KEY` = key, `X-BAPI-TIMESTAMP` = ts,
                       `X-BAPI-RECV-WINDOW` = recv, `X-BAPI-SIGN` = sig) |>
    httr2::req_timeout(20) |>
    httr2::req_retry(max_tries = 3, backoff = function(i) 2^i) |>
    httr2::req_perform()
  r <- httr2::resp_body_json(resp)
  if (!identical(as.integer(r$retCode), 0L)) stop(sprintf("%s retCode=%s msg=%s", path, r$retCode, r$retMsg))
  r
}

fld <- function(x, nm) { v <- x[[nm]]; if (is.null(v) || identical(v, "")) NA_character_ else as.character(v) }
num <- function(v) suppressWarnings(as.numeric(v))

fetch_positions <- function(key, secret) {
  r <- bybit_get(key, secret, "/v5/position/list", list(category = "linear", settleCoin = "USDT"))
  rows <- Filter(function(p) isTRUE(num(p$size) > 0), r$result$list)
  if (length(rows) == 0) return(list(upl = 0, df = NULL))
  df <- data.frame(
    symbol = vapply(rows, fld, "", "symbol"), side = vapply(rows, fld, "", "side"),
    size = vapply(rows, function(p) num(p$size), 0),
    entry_price = vapply(rows, function(p) num(p$avgPrice), 0),
    mark_price = vapply(rows, function(p) num(p$markPrice %||% p$avgPrice), 0),
    leverage = vapply(rows, function(p) num(p$leverage), 0),
    unrealised_pnl_usd = vapply(rows, function(p) num(p$unrealisedPnl %||% 0), 0),
    stringsAsFactors = FALSE)
  df$notional_usd <- df$size * df$mark_price
  list(upl = sum(df$unrealised_pnl_usd), df = df)
}

# Closed positions, paged in <=7 day chunks (Bybit's limit per query).
fetch_closed_pnl <- function(key, secret, from_ms, to_ms) {
  out <- list(); chunk <- 7 * 24 * 3600 * 1000 - 1; a <- from_ms
  while (a < to_ms) {
    b <- min(a + chunk, to_ms); cursor <- NULL
    repeat {
      q <- list(category = "linear", startTime = format(a, scientific = FALSE),
                endTime = format(b, scientific = FALSE), limit = 100)
      if (!is.null(cursor)) q$cursor <- cursor
      r <- bybit_get(key, secret, "/v5/position/closed-pnl", q)
      out <- c(out, r$result$list)
      cursor <- r$result$nextPageCursor
      if (is.null(cursor) || !nzchar(cursor)) break
    }
    a <- b + 1
  }
  out
}

sync_trades <- function(tid, key, secret, anchor) {
  last <- qs1("select max(closed_at) as t from trades where trader_id=$1", list(tid))$t[1]
  from <- if (is.na(last)) anchor else last - 2 * 86400
  lst  <- fetch_closed_pnl(key, secret, as.numeric(from) * 1000, as.numeric(Sys.time()) * 1000)
  if (length(lst) == 0) return(0L)
  df <- data.frame(
    trader_id = tid,
    order_id = vapply(lst, fld, "", "orderId"), symbol = vapply(lst, fld, "", "symbol"),
    side = vapply(lst, fld, "", "side"),
    qty = vapply(lst, function(x) num(x$closedSize %||% x$qty), 0),
    avg_entry = vapply(lst, function(x) num(x$avgEntryPrice), 0),
    avg_exit = vapply(lst, function(x) num(x$avgExitPrice), 0),
    pnl_usd = vapply(lst, function(x) num(x$closedPnl), 0),
    open_fee_usd = vapply(lst, function(x) num(x$openFee), 0),
    close_fee_usd = vapply(lst, function(x) num(x$closeFee), 0),
    leverage = vapply(lst, function(x) num(x$leverage), 0),
    closed_at = as.POSIXct(vapply(lst, function(x) num(x$updatedTime), 0) / 1000,
                           origin = "1970-01-01", tz = "UTC"),
    stringsAsFactors = FALSE)
  df <- df[!duplicated(df$order_id), ]
  con <- connect(); on.exit(dbDisconnect(con))
  have <- dbGetQuery(con, "select order_id from trades where trader_id=$1", params = list(tid))$order_id
  new  <- df[!df$order_id %in% have, ]
  if (nrow(new) > 0) dbAppendTable(con, "trades", new)
  nrow(new)
}

bot_dir <- "C:/Users/nax-trader 2/Documents/nax-monitor"
cfg   <- fromJSON(file.path(bot_dir, "config.json"), simplifyVector = FALSE)
state <- fromJSON(file.path(bot_dir, "state.json"), simplifyVector = FALSE)
bench_path <- file.path(bot_dir, "benchmarks.json")
bench <- if (file.exists(bench_path)) fromJSON(bench_path, simplifyVector = FALSE) else list(returnsPct = list())

traders <- qs1("select trader_id, display_name, starting_equity_usd, window_length_days, window_anchor from traders where not frozen")

for (i in seq_len(nrow(traders))) {
  tid <- traders$trader_id[i]
  tr_cfg <- NULL
  for (tr in cfg$traders) if (tolower(tr$name) == tid) { tr_cfg <- tr; break }
  if (is.null(tr_cfg) || is.null(state$traders[[tr_cfg$name]])) { cat(sprintf("skip %s (not in bot state)\n", tid)); next }
  bot_name <- tr_cfg$name
  snap <- state$traders[[bot_name]]$lastSnapshot
  if (is.null(snap)) { cat(sprintf("skip %s (no snapshot)\n", tid)); next }

  # Use the bot's own capture time so a stalled bot shows up as stale, not as fresh data.
  snap_time <- parse_iso(snap$capturedAt)
  if (is.na(snap_time)) { cat(sprintf("skip %s (snapshot has no timestamp)\n", tid)); next }

  anchor <- as.POSIXct(traders$window_anchor[i], tz = "UTC")
  len    <- traders$window_length_days[i]
  elapsed_days <- as.numeric(difftime(Sys.time(), anchor, units = "days"))
  if (elapsed_days < 0) { cat(sprintf("skip %s (pre-anchor)\n", tid)); next }
  n <- floor(elapsed_days / len)
  w_start <- anchor + n * len * 86400
  w_end   <- w_start + len * 86400

  # Window row. A new window starts from the previous window's closing (transfer-adjusted) equity.
  wid <- qs1("select window_id from windows where trader_id=$1 and window_start=$2", list(tid, w_start))
  if (nrow(wid) == 0) {
    prev <- qs1("select equity_adj_usd from snapshots where trader_id=$1 and captured_at <= $2
                 order by captured_at desc limit 1", list(tid, w_start))
    start_eq <- if (nrow(prev) == 1) prev$equity_adj_usd[1] else traders$starting_equity_usd[i]
    wid <- qs1("insert into windows (trader_id, window_start, window_end, starting_equity_usd)
                values ($1,$2,$3,$4) returning window_id", list(tid, w_start, w_end, start_eq))
    cat(sprintf("created window %s for %s (start equity %.2f)\n", wid$window_id[1], tid, start_eq))
  }
  wid <- wid$window_id[1]

  # ---- snapshot (skipped when the bot has not produced a new one) ----
  already <- qs1("select 1 from snapshots where trader_id=$1 and captured_at=$2", list(tid, snap_time))
  if (nrow(already) == 0) {
    equity <- as.numeric(snap$equity %||% 0)
    net_xfer <- qs1("select coalesce(sum(amount_usd),0) as x from transfers where trader_id=$1 and occurred_at >= $2",
                    list(tid, w_start))$x[1]
    eq_adj <- equity - as.numeric(net_xfer)

    # High-water mark: trails up, and resets only at a logged settlement sweep (to the balance
    # after the sweep). A trading loss never resets it.
    last_settle <- qs1("select max(occurred_at) as t from transfers where trader_id=$1 and kind='settlement'", list(tid))$t[1]
    base_ts <- if (is.na(last_settle)) anchor else max(anchor, last_settle)
    prev_hwm <- qs1("select greatest(coalesce(max(hwm_usd),0), coalesce(max(equity_adj_usd),0)) as h
                     from snapshots where trader_id=$1 and captured_at >= $2", list(tid, base_ts))$h[1]
    hwm <- max(as.numeric(prev_hwm), eq_adj)
    dd  <- if (hwm > 0) max(0, (hwm - eq_adj) / hwm * 100) else 0

    pos <- tryCatch(fetch_positions(tr_cfg$apiKey, tr_cfg$apiSecret),
                    error = function(e) { cat(sprintf("positions fetch failed for %s: %s\n", tid, conditionMessage(e))); NULL })
    sid <- qs1("insert into snapshots (trader_id, captured_at, equity_usd, equity_adj_usd, unrealised_pnl_usd,
                                       net_exposure_pct, gross_exposure_pct, max_leverage,
                                       position_count, hwm_usd, drawdown_pct, source)
                values ($1,$2,$3,$4,$5,$6,$7,$8,$9,$10,$11,'bot-sync') returning snapshot_id",
               list(tid, snap_time, equity, eq_adj, if (is.null(pos)) NA_real_ else pos$upl,
                    as.numeric(snap$netPct %||% 0), as.numeric(snap$grossPct %||% 0),
                    as.numeric(snap$maxLev %||% 0), as.integer(snap$positions %||% 0), hwm, dd))$snapshot_id[1]
    if (!is.null(pos) && !is.null(pos$df)) {
      con <- connect(); pdf <- cbind(snapshot_id = sid, pos$df)
      dbAppendTable(con, "positions", pdf); dbDisconnect(con)
    }
    cat(sprintf("synced %s: eq=$%.2f hwm=$%.2f dd=%.2f%% (bot capture %s UTC)\n",
                tid, equity, hwm, dd, format(snap_time, "%Y-%m-%d %H:%M")))
  } else {
    cat(sprintf("%s: no new bot snapshot since %s UTC\n", tid, format(snap_time, "%Y-%m-%d %H:%M")))
  }

  # ---- benchmark: only when the scrape produced a value, stamped with the scrape's own time ----
  br <- bench$returnsPct[[bot_name]]; bt <- parse_iso(bench$capturedAt)
  if (!is.null(br) && !is.na(bt)) {
    qe1("insert into benchmark_returns (trader_id, window_id, captured_at, return_pct)
         values ($1,$2,$3,$4) on conflict do nothing", list(tid, wid, bt, as.numeric(br)))
  }

  # ---- closed trades ----
  added <- tryCatch(sync_trades(tid, tr_cfg$apiKey, tr_cfg$apiSecret, anchor),
                    error = function(e) { cat(sprintf("trades sync failed for %s: %s\n", tid, conditionMessage(e))); NA })
  if (!is.na(added) && added > 0) cat(sprintf("%s: %d new closed trades\n", tid, added))
}
cat("done.\n")
