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
