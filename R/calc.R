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
eligible_payout <- function(starting_equity_usd,
                            ending_equity_usd,
                            index_return_pct,
                            profit_share_pct = 50,
                            holdback_pct     = 0) {
  net_profit <- ending_equity_usd - starting_equity_usd
  # Benchmark-equivalent floored at zero: a negative index doesn't penalise the trader.
  bench_equiv_raw <- starting_equity_usd * (index_return_pct / 100)
  bench_equiv     <- max(0, bench_equiv_raw)
  outperformance  <- net_profit - bench_equiv
  eligible <- if (net_profit > 0 && outperformance > 0) {
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
    eligible_payout_usd    = eligible,
    holdback_pct           = holdback_pct,
    holdback_usd           = holdback,
    payable_now_usd        = payable_now
  )
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
