# Rebuilds Roja's equity history from the bot's heartbeat logs and corrects Window 1/2.
# Bot log timestamps are local time (Australia/Sydney), so they are converted to UTC.
# Usage: set DRY <- FALSE below (or pass --write) to apply; default only prints.
suppressPackageStartupMessages({ library(DBI); library(RPostgres); library(jsonlite) })
setwd("C:/Users/nax-trader 2/Documents/nax-dashboard"); readRenviron(".Renviron")
args <- commandArgs(trailingOnly = TRUE)
DRY  <- !("--write" %in% args)

bot_dir   <- "C:/Users/nax-trader 2/Documents/nax-monitor"
w1_start  <- as.POSIXct("2026-09-30 00:00:00", tz = "UTC")
w1_end    <- as.POSIXct("2026-10-14 00:00:00", tz = "UTC")
initial   <- 10000
bot_name  <- "Roja"; tid <- "roja"

# ---- parse heartbeat lines ----
files <- list.files(file.path(bot_dir, "logs"), pattern = "^monitor-.*\\.log$", full.names = TRUE)
pat <- sprintf("^\\[(\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2})\\] \\[INFO\\] %s heartbeat sent: eq=([0-9.]+)", bot_name)
rows <- list()
for (f in files) {
  ln <- readLines(f, warn = FALSE, encoding = "UTF-8")
  m  <- regmatches(ln, regexec(pat, ln))
  m  <- m[lengths(m) == 3]
  if (length(m)) rows[[length(rows) + 1]] <- data.frame(
    local = vapply(m, `[`, "", 2), eq = as.numeric(vapply(m, `[`, "", 3)))
}
h <- do.call(rbind, rows)
h$t <- as.POSIXct(strptime(h$local, "%Y-%m-%d %H:%M:%S", tz = "Australia/Sydney"))
attr(h$t, "tzone") <- "UTC"
h <- h[!is.na(h$t) & h$eq > 0 & h$t >= w1_start & !duplicated(h$t), ]
h <- h[order(h$t), ]
h$hwm <- cummax(h$eq)

# Heartbeats are 15 minutes apart, so they can miss the exact top. The bot polls every 60s and
# keeps the true peak in state.json; carry it from the first heartbeat after the sampled peak.
state <- fromJSON(file.path(bot_dir, "state.json"), simplifyVector = FALSE)
state_peak <- as.numeric(state$traders[[bot_name]]$peakEquity)
pk <- which.max(h$eq)
if (state_peak > h$eq[pk] && pk < nrow(h)) {
  after <- (pk + 1):nrow(h)
  h$hwm[after] <- pmax(h$hwm[after], state_peak)
}
h$dd <- (h$hwm - h$eq) / h$hwm * 100

w1_rows   <- h[h$t <= w1_end, ]
w1_end_eq <- tail(w1_rows$eq, 1)
cat(sprintf("heartbeats: %d  from %s to %s (UTC)\n", nrow(h), format(min(h$t)), format(max(h$t))))
cat(sprintf("log peak: %.2f at %s | bot state peak: %.2f\n",
            max(h$eq), format(h$t[which.max(h$eq)]), state_peak))
cat(sprintf("window 1 close: %.2f at %s UTC (last heartbeat before %s)\n",
            w1_end_eq, format(tail(w1_rows$t, 1)), format(w1_end)))
cat(sprintf("max drawdown from high-water mark since inception: %.2f%% (lowest heartbeat %.2f at %s UTC)\n",
            max(h$dd), min(h$eq), format(h$t[which.min(h$eq)])))
cat(sprintf("largest gap between heartbeats: %.1f hours\n", max(as.numeric(diff(h$t), units = "hours"))))
if (DRY) { cat("\nDRY RUN - re-run with --write to apply.\n"); quit(save = "no") }

stopifnot(nrow(h) > 100, state_peak >= max(h$eq) - 0.01)  # the bot's 60s peak can only be >= the 15-min samples

connect <- function() dbConnect(RPostgres::Postgres(),
  host=Sys.getenv("NAX_PG_HOST"), port=as.integer(Sys.getenv("NAX_PG_PORT")),
  dbname=Sys.getenv("NAX_PG_DB"), user=Sys.getenv("NAX_PG_USER"),
  password=Sys.getenv("NAX_PG_PASSWORD"), sslmode="require", check_interrupts=FALSE)
con <- connect()

dbExecute(con, "delete from snapshots where trader_id=$1 and source in ('bot-log','hwm-seed')", params = list(tid))
ins <- data.frame(trader_id = tid, captured_at = h$t, equity_usd = h$eq, equity_adj_usd = h$eq,
                  hwm_usd = h$hwm, drawdown_pct = h$dd, source = "bot-log")
dbAppendTable(con, "snapshots", ins)
cat(sprintf("inserted %d bot-log snapshots\n", nrow(ins)))

w1 <- dbGetQuery(con, "select window_id from windows where trader_id=$1 and window_start=$2", params = list(tid, w1_start))
w2 <- dbGetQuery(con, "select window_id from windows where trader_id=$1 and window_start=$2", params = list(tid, w1_end))
net <- w1_end_eq - initial

# Window 1: correct the closing equity; the index return was never recorded, so it is NULL, not estimated.
dbExecute(con, "update payouts set ending_equity_usd=$2, net_profit_usd=$3, index_return_pct=NULL,
                benchmark_equiv_usd=0, outperformance_usd=$3, eligible_payout_usd=0, payable_now_usd=0
                where window_id=$1", params = list(w1$window_id[1], w1_end_eq, net))
dbExecute(con, "delete from benchmark_returns where window_id=$1", params = list(w1$window_id[1]))
dbExecute(con, "update payout_ledger set notes=$2 where payout_id in (select payout_id from payouts where window_id=$1)",
          params = list(w1$window_id[1],
            "Window 1 payout is $0: net trading profit was negative. Index return for this window was not recorded."))
# Window 2 starts from Window 1's true closing equity.
dbExecute(con, "update windows set starting_equity_usd=$2 where window_id=$1", params = list(w2$window_id[1], w1_end_eq))
cat(sprintf("window 1 close set to %.2f (net %.2f); window 2 start set to %.2f\n", w1_end_eq, net, w1_end_eq))
dbDisconnect(con)
