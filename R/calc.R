# NAX calc module. Pure functions. No DB, no IO.
# Covered by tests/test-calc.R. Change nothing here without updating tests.

# --- window math --------------------------------------------------------------

#' Return the [start, end) UTC bounds of the rolling window containing `at`.
#' Windows are back-to-back: next_start == prev_end.
window_bounds <- function(anchor_utc, length_days, at_utc) {
  anchor <- as.POSIXct(anchor_utc, tz = "UTC")
  at     <- as.POSIXct(at_utc,     tz = "UTC")
  if (at < anchor) return(list(start = anchor, end = anchor + length_days * 86400))
  elapsed_days <- as.numeric(difftime(at, anchor, units = "days"))
  n <- floor(elapsed_days / length_days)
  start <- anchor + n * length_days * 86400
  end   <- start  +     length_days * 86400
  list(start = start, end = end)
}

# --- transfer-adjusted equity -------------------------------------------------

#' Sum of transfer amounts (deposits + outbound settlements) within [start, at].
#' Positive = deposit (user added capital), negative = withdrawal.
net_transfers_since <- function(transfers_df, start_utc, at_utc) {
  if (is.null(transfers_df) || nrow(transfers_df) == 0) return(0)
  t <- transfers_df[transfers_df$occurred_at >= start_utc &
                    transfers_df$occurred_at <= at_utc, ]
  if (nrow(t) == 0) return(0)
  sum(t$amount_usd)
}

#' Equity with transfers backed out, so a settlement sweep is not a loss.
#' equity_adj = raw_equity - net_transfers_since_window_start
transfer_adjusted_equity <- function(raw_equity, transfers_df, window_start_utc, at_utc) {
  raw_equity - net_transfers_since(transfers_df, window_start_utc, at_utc)
}

# --- trailing HWM and drawdown ------------------------------------------------

#' Trailing high-water mark over the current window.
#' Resets to the post-sweep balance at each settlement (handled by the writer
#' when it starts a new window row; here we just take the max of adj equities).
trailing_hwm <- function(adj_equity_series) {
  if (length(adj_equity_series) == 0) return(NA_real_)
  cummax_val <- max(adj_equity_series, na.rm = TRUE)
  cummax_val
}

#' Drawdown from HWM, as a positive percent. 0 if HWM is 0 or NA.
drawdown_pct <- function(hwm, adj_equity) {
  if (is.na(hwm) || hwm <= 0) return(0)
  max(0, (hwm - adj_equity) / hwm * 100)
}

# --- exposure and leverage ----------------------------------------------------

#' Positions: data.frame with columns side ('Buy'|'Sell'), size, mark_price.
exposure <- function(positions_df, equity) {
  if (is.null(positions_df) || nrow(positions_df) == 0 || equity <= 0) {
    return(list(net_pct = 0, gross_pct = 0, max_leverage = 0))
  }
  notional <- positions_df$size * positions_df$mark_price
  signed   <- ifelse(positions_df$side == "Buy", notional, -notional)
  list(
    net_pct      = sum(signed)        / equity * 100,
    gross_pct    = sum(abs(notional)) / equity * 100,
    max_leverage = if ("leverage" %in% names(positions_df)) max(positions_df$leverage, na.rm = TRUE) else 0
  )
}

# --- benchmark ----------------------------------------------------------------

#' Index return over a window, in percent, using 23:55 UTC snapshots.
#' Rebase: window-start level becomes the denominator; a window spanning a
#' rebalance uses the basket in effect at window start (caller supplies that
#' series).
benchmark_return_pct <- function(index_start_level, index_end_level) {
  if (is.na(index_start_level) || index_start_level <= 0) return(NA_real_)
  (index_end_level / index_start_level - 1) * 100
}

# --- eligible payout ----------------------------------------------------------

#' Eligible payout per spec. Returns a list with every intermediate value so
#' the UI can show the working.
#'
#' @param starting_equity_usd equity at window start (transfer-adjusted)
#' @param ending_equity_usd   equity at window end   (transfer-adjusted, pre-sweep)
#' @param index_return_pct    NAX index return over the window, in percent
#' @param profit_share_pct    default 50
#' @param holdback_pct        default 0; non-zero defers that fraction one window
#' @param initial_capital_usd   trader's initial funded capital (enables the cumulative gate)
#' @param cumulative_equity_usd equity since inception, transfer-adjusted (i.e. including
#'   anything already swept out). The gate requires this to be strictly above
#'   initial_capital_usd, so losses from earlier windows must be recovered first.
#'   Leave either NULL to skip the gate.
eligible_payout <- function(starting_equity_usd,
                            ending_equity_usd,
                            index_return_pct,
                            profit_share_pct = 50,
                            holdback_pct     = 0,
                            initial_capital_usd   = NULL,
                            cumulative_equity_usd = NULL) {
  net_profit <- ending_equity_usd - starting_equity_usd
  # Benchmark-equivalent floored at zero: a negative index doesn't penalise the trader.
  bench_equiv_raw <- starting_equity_usd * (index_return_pct / 100)
  bench_equiv     <- max(0, bench_equiv_raw)
  outperformance  <- net_profit - bench_equiv

  gate_applies   <- !is.null(initial_capital_usd) && !is.null(cumulative_equity_usd)
  cumulative_pnl <- if (gate_applies) cumulative_equity_usd - initial_capital_usd else NA_real_
  above_initial  <- if (gate_applies) cumulative_pnl > 0 else TRUE
  shortfall      <- if (gate_applies && !above_initial) -cumulative_pnl else 0

  eligible <- if (net_profit > 0 && outperformance > 0 && above_initial) {
    outperformance * (profit_share_pct / 100)
  } else 0
  holdback    <- eligible * (holdback_pct / 100)
  payable_now <- eligible - holdback
  list(
    net_profit_usd         = net_profit,
    index_return_pct       = index_return_pct,
    benchmark_equiv_raw    = bench_equiv_raw,
    benchmark_equiv_usd    = bench_equiv,
    outperformance_usd     = outperformance,
    profit_share_pct       = profit_share_pct,
    cumulative_gate_applies = gate_applies,
    cumulative_pnl_usd     = cumulative_pnl,
    above_initial_capital  = above_initial,
    shortfall_to_initial_usd = shortfall,
    eligible_payout_usd    = eligible,
    holdback_pct           = holdback_pct,
    holdback_usd           = holdback,
    payable_now_usd        = payable_now
  )
}

# --- performance analytics ----------------------------------------------------

#' Largest peak-to-trough fall in an equity series, as a positive percent.
max_drawdown_pct <- function(equity) {
  equity <- equity[!is.na(equity)]
  if (length(equity) == 0) return(0)
  peak <- cummax(equity)
  max(0, (peak - equity) / peak * 100)
}

#' Gain needed from `equity` to get back to `target` (e.g. HWM or initial capital), in percent.
recovery_needed_pct <- function(equity, target) {
  if (is.na(equity) || equity <= 0 || target <= equity) return(0)
  (target / equity - 1) * 100
}

#' Daily P&L from end-of-day equity closes. The first day is measured from start_equity.
daily_pnl <- function(day, close, start_equity) {
  if (length(close) == 0) {
    return(data.frame(day = as.Date(character()), close = numeric(),
                      pnl_usd = numeric(), ret_pct = numeric()))
  }
  o    <- order(day)
  day  <- day[o]; close <- close[o]
  prev <- c(start_equity, close[-length(close)])
  data.frame(day = day, close = close, pnl_usd = close - prev,
             ret_pct = (close - prev) / prev * 100)
}

daily_stats <- function(daily) {
  n <- nrow(daily)
  if (n == 0) {
    return(list(n_days = 0, best_usd = NA_real_, best_day = as.Date(NA),
                worst_usd = NA_real_, worst_day = as.Date(NA),
                avg_ret_pct = NA_real_, sd_ret_pct = NA_real_, pct_up_days = NA_real_))
  }
  list(n_days      = n,
       best_usd    = max(daily$pnl_usd),   best_day  = daily$day[which.max(daily$pnl_usd)],
       worst_usd   = min(daily$pnl_usd),   worst_day = daily$day[which.min(daily$pnl_usd)],
       avg_ret_pct = mean(daily$ret_pct),
       sd_ret_pct  = if (n >= 2) sd(daily$ret_pct) else NA_real_,
       pct_up_days = mean(daily$pnl_usd > 0) * 100)
}

#' Closed-trade statistics from a vector of realised P&L per trade.
trade_stats <- function(pnl) {
  n <- length(pnl)
  if (n == 0) {
    return(list(n = 0, wins = 0, losses = 0, win_rate = NA_real_, avg_win = NA_real_,
                avg_loss = NA_real_, payoff_ratio = NA_real_, profit_factor = NA_real_,
                expectancy = NA_real_, largest_win = NA_real_, largest_loss = NA_real_,
                total_pnl = 0))
  }
  w <- pnl[pnl > 0]; l <- pnl[pnl < 0]
  avg_win  <- if (length(w)) mean(w) else NA_real_
  avg_loss <- if (length(l)) mean(l) else NA_real_
  list(n = n, wins = length(w), losses = length(l),
       win_rate      = length(w) / n * 100,
       avg_win       = avg_win,
       avg_loss      = avg_loss,
       payoff_ratio  = if (!is.na(avg_win) && !is.na(avg_loss)) avg_win / abs(avg_loss) else NA_real_,
       profit_factor = if (length(l)) sum(w) / -sum(l) else NA_real_,
       expectancy    = mean(pnl),
       largest_win   = if (length(w)) max(w) else NA_real_,
       largest_loss  = if (length(l)) min(l) else NA_real_,
       total_pnl     = sum(pnl))
}

#' Realised P&L and trade count per symbol, best first.
pnl_by_symbol <- function(symbol, pnl) {
  if (length(pnl) == 0) return(data.frame(symbol = character(), pnl_usd = numeric(), trades = integer()))
  agg <- aggregate(pnl, by = list(symbol = symbol), FUN = sum)
  cnt <- table(symbol)
  out <- data.frame(symbol = agg$symbol, pnl_usd = agg$x,
                    trades = as.integer(cnt[agg$symbol]), stringsAsFactors = FALSE)
  out[order(-out$pnl_usd), ]
}

# --- verdict track ------------------------------------------------------------

#' Window advancement verdict given interim or final state.
#' Pass: trader_ret > index_ret, trader_ret > 0, zero critical, major <= allowance, soft <= allowance.
verdict <- function(trader_return_pct, index_return_pct,
                    critical_count, major_count, soft_count,
                    max_critical = 0, max_major = 1, max_soft = 1) {
  breach_ok <- critical_count <= max_critical &&
               major_count    <= max_major    &&
               soft_count     <= max_soft
  if (!breach_ok) return(list(status = "FAIL", reason = "breach allowance exceeded"))
  if (trader_return_pct <= 0) return(list(status = "FAIL", reason = "trader return not positive"))
  if (is.na(index_return_pct)) return(list(status = "PENDING", reason = "index unavailable"))
  if (trader_return_pct > index_return_pct) {
    list(status = "PASS", reason = sprintf("beating index by %.2f pp", trader_return_pct - index_return_pct))
  } else {
    list(status = "BEHIND", reason = sprintf("trailing index by %.2f pp", index_return_pct - trader_return_pct))
  }
}
