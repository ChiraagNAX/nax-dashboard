library(testthat)
source("../R/calc.R")

test_that("eligible_payout — spec example 1: trader +8%, index +5%", {
  r <- eligible_payout(10000, 10800, 5)
  expect_equal(r$net_profit_usd,       800)
  expect_equal(r$benchmark_equiv_usd,  500)
  expect_equal(r$outperformance_usd,   300)
  expect_equal(r$eligible_payout_usd,  150)
})

test_that("eligible_payout — spec example 2: trader +2%, index -3% (floored)", {
  r <- eligible_payout(10000, 10200, -3)
  expect_equal(r$net_profit_usd,       200)
  expect_equal(r$benchmark_equiv_raw, -300)
  expect_equal(r$benchmark_equiv_usd,    0)
  expect_equal(r$outperformance_usd,   200)
  expect_equal(r$eligible_payout_usd,  100)
})

test_that("eligible_payout — spec example 3: trader -1%, payout 0", {
  r <- eligible_payout(10000, 9900, 2)
  expect_equal(r$eligible_payout_usd, 0)
})

test_that("eligible_payout — spec example 4: trader +4%, index +6%, payout 0", {
  r <- eligible_payout(10000, 10400, 6)
  expect_equal(r$outperformance_usd, -200)
  expect_equal(r$eligible_payout_usd,   0)
})

test_that("eligible_payout — holdback splits payable_now vs holdback", {
  r <- eligible_payout(10000, 10800, 5, holdback_pct = 25)
  expect_equal(r$eligible_payout_usd, 150)
  expect_equal(r$holdback_usd,         37.5)
  expect_equal(r$payable_now_usd,     112.5)
})

test_that("cumulative gate — profitable window but still below initial capital pays nothing", {
  # Roja-style: window started at 8,862.53 after an earlier loss, now 9,172.80, initial 10,000.
  r <- eligible_payout(8862.53, 9172.80, -1.8,
                       initial_capital_usd = 10000, cumulative_equity_usd = 9172.80)
  expect_gt(r$net_profit_usd, 0)
  expect_gt(r$outperformance_usd, 0)
  expect_false(r$above_initial_capital)
  expect_equal(r$eligible_payout_usd, 0)
  expect_equal(r$shortfall_to_initial_usd, 10000 - 9172.80)
})

test_that("cumulative gate — pays once cumulative equity is above initial capital", {
  r <- eligible_payout(8862.53, 10062.53, 0,
                       initial_capital_usd = 10000, cumulative_equity_usd = 10062.53)
  expect_true(r$above_initial_capital)
  expect_equal(r$cumulative_pnl_usd, 62.53)
  expect_equal(r$eligible_payout_usd, 600)
})

test_that("cumulative gate — exactly at initial capital is not above it", {
  r <- eligible_payout(9000, 10000, 0,
                       initial_capital_usd = 10000, cumulative_equity_usd = 10000)
  expect_equal(r$eligible_payout_usd, 0)
})

test_that("cumulative gate — equity already swept out still counts toward cumulative", {
  # Equity 9,700 in account plus 1,200 previously swept = 10,900 cumulative (> 10,000).
  r <- eligible_payout(9500, 9700, 0,
                       initial_capital_usd = 10000, cumulative_equity_usd = 10900)
  expect_true(r$above_initial_capital)
  expect_equal(r$eligible_payout_usd, 100)
})

test_that("max_drawdown_pct finds the deepest peak-to-trough fall", {
  expect_equal(max_drawdown_pct(c(100, 110, 99, 105, 120, 108)), 10)
  expect_equal(max_drawdown_pct(c(100, 101, 102)), 0)
  expect_equal(max_drawdown_pct(numeric()), 0)
})

test_that("recovery_needed_pct is the gain needed to return to a target", {
  expect_equal(recovery_needed_pct(9000, 10000), (10000 / 9000 - 1) * 100)
  expect_equal(recovery_needed_pct(10500, 10000), 0)
})

test_that("daily_pnl measures day one from the starting equity", {
  d <- daily_pnl(as.Date(c("2026-10-01", "2026-10-02", "2026-10-03")),
                 c(10100, 9999, 10200), start_equity = 10000)
  expect_equal(d$pnl_usd, c(100, -101, 201))
  expect_equal(d$ret_pct[1], 1)
  s <- daily_stats(d)
  expect_equal(s$best_usd, 201)
  expect_equal(s$worst_usd, -101)
  expect_equal(s$pct_up_days, 2 / 3 * 100)
})

test_that("trade_stats — win rate, payoff, profit factor, expectancy", {
  s <- trade_stats(c(100, -50, 200, -50, 0))
  expect_equal(s$n, 5)
  expect_equal(s$wins, 2)
  expect_equal(s$losses, 2)
  expect_equal(s$win_rate, 40)
  expect_equal(s$avg_win, 150)
  expect_equal(s$avg_loss, -50)
  expect_equal(s$payoff_ratio, 3)
  expect_equal(s$profit_factor, 3)
  expect_equal(s$expectancy, 40)
  expect_equal(s$largest_win, 200)
  expect_equal(s$largest_loss, -50)
})

test_that("trade_stats copes with no trades and with no losing trades", {
  expect_equal(trade_stats(numeric())$n, 0)
  s <- trade_stats(c(10, 20))
  expect_true(is.na(s$profit_factor))
  expect_equal(s$win_rate, 100)
})

test_that("pnl_by_symbol aggregates and sorts best first", {
  o <- pnl_by_symbol(c("BTC", "ETH", "BTC"), c(100, -30, -20))
  expect_equal(o$symbol, c("BTC", "ETH"))
  expect_equal(o$pnl_usd, c(80, -30))
  expect_equal(o$trades, c(2L, 1L))
})

test_that("drawdown — Level 1 HWM $10,800 triggers soft at $9,828 not hard", {
  hwm <- 10800
  # soft 9% -> $9,828; hard 13% -> $9,396
  expect_equal(drawdown_pct(hwm, 9828), 9)
  expect_equal(drawdown_pct(hwm, 9396), 13)
  # equity at $10,000 after HWM of $10,800
  expect_equal(round(drawdown_pct(hwm, 10000), 4), round((800/10800)*100, 4))
})

test_that("drawdown — $1,000 loss from $10,000 HWM is 10% (soft but not hard)", {
  expect_equal(drawdown_pct(10000, 9000), 10)
})

test_that("transfer-adjusted equity backs out a settlement sweep", {
  transfers <- data.frame(
    occurred_at = as.POSIXct(c("2026-10-14 00:00:00"), tz = "UTC"),
    amount_usd  = c(-300)   # outbound sweep
  )
  wstart <- as.POSIXct("2026-10-14 00:00:00", tz = "UTC")
  at     <- as.POSIXct("2026-10-14 00:05:00", tz = "UTC")
  # raw equity dropped by 300 due to sweep; adjusted should look flat
  expect_equal(transfer_adjusted_equity(10500, transfers, wstart, at), 10800)
})

test_that("trailing HWM takes the max of a series", {
  expect_equal(trailing_hwm(c(10000, 10200, 10800, 10500, 10700)), 10800)
})

test_that("window_bounds — Roja: anchor 2026-09-30, length 14, closes 14 Oct 00:00 UTC", {
  anchor <- "2026-09-30T00:00:00Z"
  b1 <- window_bounds(anchor, 14, "2026-10-07T12:00:00Z")
  expect_equal(format(b1$start, "%Y-%m-%d %H:%M", tz = "UTC"), "2026-09-30 00:00")
  expect_equal(format(b1$end,   "%Y-%m-%d %H:%M", tz = "UTC"), "2026-10-14 00:00")
})

test_that("exposure — single long position", {
  pos <- data.frame(side = "Buy", size = 1, mark_price = 50000, leverage = 3)
  e <- exposure(pos, equity = 10000)
  expect_equal(e$net_pct,      500)
  expect_equal(e$gross_pct,    500)
  expect_equal(e$max_leverage,   3)
})

test_that("verdict — pass when trader beats index with no excess breaches", {
  v <- verdict(8, 5, 0, 0, 0)
  expect_equal(v$status, "PASS")
})

test_that("verdict — fail when critical breach present", {
  v <- verdict(8, 5, 1, 0, 0)
  expect_equal(v$status, "FAIL")
})

test_that("verdict — fail when trader return not positive even if beating a worse index", {
  v <- verdict(-1, -5, 0, 0, 0)
  expect_equal(v$status, "FAIL")
})
