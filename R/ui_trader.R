# Trader view - bslib layout, status-aware colors, financial dashboard feel

BENCH_MAX_AGE_H <- 6

trader_ui <- function(id = "trader") {
  ns <- NS(id)
  tagList(
    uiOutput(ns("header")),
    uiOutput(ns("staleness_banner")),

    div(class = "nax-grid k4",
      uiOutput(ns("kpi_equity")), uiOutput(ns("kpi_return")),
      uiOutput(ns("kpi_drawdown")), uiOutput(ns("kpi_vs_index"))),
    div(class = "nax-grid k4",
      uiOutput(ns("kpi_cumulative")), uiOutput(ns("kpi_maxdd")),
      uiOutput(ns("kpi_winrate")), uiOutput(ns("kpi_pf"))),

    div(class = "nax-grid c21",
      card(card_header("Equity vs HWM"),
           card_body(plotOutput(ns("dd_plot"), height = "280px"))),
      card(card_header("Risk limits"),
           card_body(uiOutput(ns("limits_body"))))),

    div(class = "nax-grid c3",
      card(card_header("Breaches (this window)"),
           card_body(uiOutput(ns("breaches_body")))),
      card(card_header("Benchmark vs NAX Index"),
           card_body(uiOutput(ns("benchmark_body")))),
      card(card_header("Indicative payout"),
           card_body(uiOutput(ns("payout_body"))))),

    # Performance analysis
    div(class = "nax-section-head",
        tags$h5("Performance analysis"),
        radioButtons(ns("scope"), NULL, inline = TRUE,
                     choices = c("This window" = "window", "Since inception" = "all"),
                     selected = "all")),
    navset_card_tab(
      nav_panel("Daily P&L",
        plotOutput(ns("daily_plot"), height = "260px"),
        uiOutput(ns("daily_stats_ui"))),
      nav_panel("P&L breakdown", uiOutput(ns("breakdown_ui"))),
      nav_panel("Trade statistics", uiOutput(ns("trade_stats_ui"))),
      nav_panel("By symbol",
        plotOutput(ns("symbol_plot"), height = "260px"),
        tableOutput(ns("symbol_table"))),
      nav_panel("Recent trades", tableOutput(ns("recent_trades_table")))
    ),

    div(class = "nax-grid c2", style = "margin-top:16px;",
      card(card_header("Open positions"), card_body(uiOutput(ns("positions_body")))),
      card(card_header("Payout ledger"), card_body(uiOutput(ns("ledger_body"))))),

    div(class = "nax-footer",
      "Indicative only. Payouts are determined by NAX reconciliation under your Contractor Agreement.",
      br(),
      tags$a("Programme document", href = "#"), " · ",
      tags$a("Index methodology", href = "https://k95y82-chiraag-nax.shinyapps.io/nax-crypto-index", target = "_blank"), " · ",
      tags$a("How this is calculated", href = "#")
    )
  )
}

trader_server <- function(id, trader_id_rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    tick <- reactiveTimer(30000)

    bundle <- reactive({
      tick()
      tid <- trader_id_rv()
      req(tid)
      trader   <- get_trader(tid)
      window   <- get_current_window(tid)
      snap     <- get_latest_snapshot(tid)
      bench    <- if (nrow(window) == 1) get_latest_benchmark(tid, window$window_id) else data.frame()
      breaches <- if (nrow(window) == 1) get_breaches_in_window(tid, window$window_id) else data.frame()
      pos      <- if (nrow(snap)   == 1) get_positions_for_snapshot(snap$snapshot_id) else data.frame()
      series   <- if (nrow(window) == 1) get_equity_series(tid, window$window_start) else data.frame()
      cum_equity <- get_cumulative_equity(tid)
      # An index figure older than BENCH_MAX_AGE_H is not shown, rather than presented as current.
      bench_age_h <- if (nrow(bench) == 1) as.numeric(difftime(Sys.time(), bench$captured_at, units = "hours")) else NA_real_
      if (nrow(bench) == 1 && bench_age_h > BENCH_MAX_AGE_H) bench <- bench[0, , drop = FALSE]
      list(trader = trader, window = window, snap = snap, bench = bench,
           breaches = breaches, pos = pos, series = series,
           cum_equity = cum_equity, bench_age_h = bench_age_h)
    })

    # ---------- HEADER ----------
    output$header <- renderUI({
      b <- bundle(); t <- b$trader; w <- b$window; s <- b$snap
      if (nrow(t) == 0) return(div("Trader not found."))
      lvl_badge <- span(class = "badge-level", t$level_code)
      entity <- if (!is.na(t$entity_name)) sprintf(" · %s", t$entity_name) else ""
      capital <- sprintf("$%s capital", format(t$capital_usd, big.mark = ","))

      days_elapsed <- 0; days_total <- 14; progress_pct <- 0; window_line <- "No active window"
      if (nrow(w) == 1) {
        days_total <- as.numeric(difftime(w$window_end, w$window_start, units = "days"))
        days_elapsed <- as.numeric(difftime(Sys.time(), w$window_start, units = "days"))
        days_left <- max(0, days_total - days_elapsed)
        progress_pct <- min(100, max(0, days_elapsed / days_total * 100))
        window_line <- sprintf("Window %s → %s UTC · day %.1f/%.0f · %.1f days left",
                               format(w$window_start, "%d %b"),
                               format(w$window_end, "%d %b"),
                               days_elapsed, days_total, days_left)
      }
      stamp <- if (nrow(s) == 1) {
        age_min <- as.numeric(difftime(Sys.time(), s$captured_at, units = "mins"))
        sprintf("Last updated %s UTC · %.0f min ago",
                format(s$captured_at, "%H:%M:%S"), age_min)
      } else "Waiting for first snapshot"

      chip <- if (nrow(s) == 1) {
        age <- as.numeric(difftime(Sys.time(), s$captured_at, units = "mins"))
        if (age <= 5) span(class = "nax-chip live", "Live")
        else if (age <= 20) span(class = "nax-chip delayed", "Delayed")
        else span(class = "nax-chip stale", "Stale")
      }
      div(class = "nax-header",
        div(class = "top", tags$h2(t$display_name, lvl_badge), chip),
        div(class = "sub", HTML(sprintf("%s %s · %s", capital, entity, window_line))),
        div(class = "sub", stamp),
        div(class = "nax-progress-track",
          div(class = "nax-progress-fill",
              style = sprintf("width:%.0f%%", progress_pct)))
      )
    })

    # ---------- STALENESS ----------
    output$staleness_banner <- renderUI({
      s <- bundle()$snap
      if (nrow(s) == 0) return(NULL)
      age_min <- as.numeric(difftime(Sys.time(), s$captured_at, units = "mins"))
      if (age_min > 20) {
        div(class = "nax-stale",
          sprintf("⚠ Data is %.0f minutes old. Values may not reflect the live account.", age_min))
      }
    })

    # ---------- KPIs ----------
    output$kpi_equity <- renderUI({
      s <- bundle()$snap
      if (nrow(s) == 0) return(nax_kpi("Equity", "n/a"))
      nax_kpi("Equity (USD)",
              sprintf("$%s", format(round(s$equity_adj_usd, 0), big.mark = ",")),
              sprintf("Unrealised %s$%s",
                      if ((s$unrealised_pnl_usd %||% 0) >= 0) "+" else "",
                      format(round(s$unrealised_pnl_usd %||% 0, 0), big.mark = ",")))
    })

    output$kpi_return <- renderUI({
      b <- bundle(); s <- b$snap; w <- b$window
      if (nrow(s) == 0 || nrow(w) == 0) return(nax_kpi("Window return", "n/a"))
      start_eq <- w$starting_equity_usd
      ret_usd  <- s$equity_adj_usd - start_eq
      ret_pct  <- ret_usd / start_eq * 100
      status <- if (ret_pct > 0) "ok" else if (ret_pct < 0) "bad" else "ok"
      arrow <- if (ret_pct >= 0) "▲" else "▼"
      cls   <- if (ret_pct >= 0) "delta-up" else "delta-down"
      nax_kpi("Window return",
              sprintf("%s%.2f%%", if (ret_pct >= 0) "+" else "", ret_pct),
              sprintf("<span class='%s'>%s $%s</span>",
                      cls, arrow,
                      format(abs(round(ret_usd, 0)), big.mark = ",")),
              status)
    })

    output$kpi_drawdown <- renderUI({
      b <- bundle(); s <- b$snap; t <- b$trader
      if (nrow(s) == 0) return(nax_kpi("Drawdown", "n/a"))
      dd <- s$drawdown_pct %||% 0
      pct_of_hard <- dd / t$hard_stop_pct * 100
      status <- status_of_pct(pct_of_hard)
      nax_kpi("Drawdown",
              sprintf("%.2f%%", dd),
              sprintf("Hard stop %.0f%% · HWM $%s",
                      t$hard_stop_pct,
                      format(round(s$hwm_usd %||% 0, 0), big.mark = ",")),
              status)
    })

    output$kpi_vs_index <- renderUI({
      b <- bundle(); s <- b$snap; w <- b$window; bm <- b$bench
      if (nrow(s) == 0 || nrow(w) == 0) return(nax_kpi("vs Index", "n/a"))
      start_eq <- w$starting_equity_usd
      ret_pct  <- (s$equity_adj_usd - start_eq) / start_eq * 100
      if (nrow(bm) == 0) return(nax_kpi("vs Index", "n/a", "Awaiting index"))
      diff <- ret_pct - bm$return_pct
      status <- if (diff > 0) "ok" else "bad"
      arrow <- if (diff >= 0) "▲" else "▼"
      cls   <- if (diff >= 0) "delta-up" else "delta-down"
      nax_kpi("vs NAX Index",
              sprintf("%s%.2f pp", if (diff >= 0) "+" else "", diff),
              sprintf("<span class='%s'>%s Index %+.2f%%</span>",
                      cls, arrow, bm$return_pct),
              status)
    })

    # ---------- LIMITS GAUGES ----------
    output$limits_body <- renderUI({
      b <- bundle(); s <- b$snap; t <- b$trader
      if (nrow(s) == 0) return(div("Waiting for snapshot."))
      net <- s$net_exposure_pct %||% 0
      gross <- s$gross_exposure_pct %||% 0
      lev <- s$max_leverage %||% 0
      dd  <- s$drawdown_pct %||% 0
      tagList(
        nax_gauge("Net exposure",
                  sprintf("%.1f%% / %.0f%%", net, t$max_net_pct),
                  abs(net) / t$max_net_pct * 100),
        nax_gauge("Gross exposure",
                  sprintf("%.1f%% / %.0f%%", gross, t$max_gross_pct),
                  gross / t$max_gross_pct * 100),
        nax_gauge("Max leverage",
                  sprintf("%.1fx / %.0fx", lev, t$max_leverage),
                  lev / t$max_leverage * 100),
        nax_gauge("Drawdown",
                  sprintf("%.1f%% / %.0f%%", dd, t$hard_stop_pct),
                  dd / t$hard_stop_pct * 100),
        div(style = "margin-top: 10px; font-size: 11px; color: #94A3B8;",
            sprintf("Soft DD threshold %.0f%% · Hard stop %.0f%%",
                    t$soft_dd_pct, t$hard_stop_pct))
      )
    })

    # ---------- EQUITY CHART ----------
    output$dd_plot <- renderPlot({
      b <- bundle(); s <- b$series; t <- b$trader
      if (nrow(s) == 0) {
        plot.new()
        text(0.5, 0.5, "Waiting for data...", col = "#94A3B8", cex = 1.1)
        return(invisible())
      }
      hwm_now <- max(s$hwm_usd, na.rm = TRUE)
      soft <- hwm_now * (1 - t$soft_dd_pct   / 100)
      hard <- hwm_now * (1 - t$hard_stop_pct / 100)
      op <- par(mar = c(2.8, 4.2, 1, 1), family = "sans", bg = "white",
                col.axis = "#64748B", col.lab = "#64748B", cex.axis = 0.85)
      on.exit(par(op))
      ymin <- min(c(s$equity_adj_usd, hard), na.rm = TRUE) * 0.995
      ymax <- max(c(s$equity_adj_usd, s$hwm_usd), na.rm = TRUE) * 1.005
      plot(s$captured_at, s$equity_adj_usd, type = "n",
           xlab = "", ylab = "USD", ylim = c(ymin, ymax),
           bty = "n", xaxt = "n")
      axis.POSIXct(1, s$captured_at, format = "%d %b", col = "#CBD5E1", col.ticks = "#CBD5E1")
      grid(col = "#F1F5F9", lty = 1)
      rect(min(s$captured_at), ymin, max(s$captured_at), hard,
           col = "#FEE2E2", border = NA)
      rect(min(s$captured_at), hard, max(s$captured_at), soft,
           col = "#FEF3C7", border = NA)
      abline(h = soft, col = "#F59E0B", lty = 2, lwd = 1)
      abline(h = hard, col = "#EF4444", lty = 2, lwd = 1)
      lines(s$captured_at, s$hwm_usd, col = "#64748B", lwd = 1.5, lty = 3)
      lines(s$captured_at, s$equity_adj_usd, col = "#0EA5E9", lwd = 2.5)
      points(tail(s$captured_at, 1), tail(s$equity_adj_usd, 1),
             pch = 19, col = "#0EA5E9", cex = 1.2)
      legend("topleft", bty = "n", cex = 0.85,
             lty = c(1, 3, 2, 2), lwd = c(2.5, 1.5, 1, 1),
             col = c("#0EA5E9", "#64748B", "#F59E0B", "#EF4444"),
             legend = c("Equity", "HWM", "Soft threshold", "Hard stop"))
    })

    # ---------- BREACHES ----------
    output$breaches_body <- renderUI({
      b <- bundle(); br <- b$breaches
      c_count <- sum(br$category == "critical")
      m_count <- sum(br$category == "major")
      s_count <- sum(br$category == "soft")
      tagList(
        nax_tally(c_count, m_count, s_count),
        if (nrow(br) > 0) {
          tagList(h6("Events", style = "font-size:12px; color:#64748B; text-transform:uppercase; letter-spacing:0.05em; margin-top:12px;"),
                  tableOutput(ns("breach_table")))
        } else {
          div(style = "color:#64748B; font-size:13px; padding:6px 0;",
              "No breaches recorded this window.")
        }
      )
    })

    output$breach_table <- renderTable({
      br <- bundle()$breaches
      if (nrow(br) == 0) return(NULL)
      data.frame(
        When     = format(br$occurred_at, "%d %b %H:%M"),
        Category = toupper(br$category),
        Event    = br$kind
      )
    }, striped = FALSE, bordered = FALSE, spacing = "xs")

    # ---------- BENCHMARK ----------
    output$benchmark_body <- renderUI({
      b <- bundle(); w <- b$window; s <- b$snap; bm <- b$bench
      if (nrow(w) == 0 || nrow(s) == 0) return(div("Waiting."))
      start_eq <- w$starting_equity_usd
      ret_pct  <- (s$equity_adj_usd - start_eq) / start_eq * 100
      idx_pct  <- if (nrow(bm) == 1) bm$return_pct else NA
      diff     <- if (!is.na(idx_pct)) ret_pct - idx_pct else NA
      v <- verdict(ret_pct, idx_pct %||% NA_real_,
                   sum(b$breaches$category == "critical"),
                   sum(b$breaches$category == "major"),
                   sum(b$breaches$category == "soft"))

      tagList(
        div(class = "nax-payout-row",
          span(class = "k", "Trader return"),
          span(class = "v nax-mono", sprintf("%+.2f%%", ret_pct))),
        div(class = "nax-payout-row",
          span(class = "k", "Index return"),
          span(class = "v nax-mono",
               if (is.na(idx_pct)) "n/a" else sprintf("%+.2f%%", idx_pct))),
        div(class = "nax-payout-row",
          span(class = "k", "Difference"),
          span(class = "v nax-mono",
               if (is.na(diff)) "n/a" else sprintf("%+.2f pp", diff))),
        if (nrow(bm) == 0)
          div(style = "font-size:11px; color:#94A3B8; margin-top:6px;",
              if (is.na(b$bench_age_h)) "Index data is not available yet."
              else sprintf("The latest index figure is %.0f hours old, so it is not shown.", b$bench_age_h)),
        div(style = "margin-top: 14px;",
          div(style = "font-size: 11px; color:#64748B; text-transform:uppercase; letter-spacing:0.05em; margin-bottom:6px;",
              "Verdict track"),
          nax_verdict_badge(v$status),
          div(style = "font-size: 12px; color:#64748B; margin-top:6px;", v$reason))
      )
    })

    # ---------- PAYOUT ----------
    output$payout_body <- renderUI({
      b <- bundle(); w <- b$window; s <- b$snap; bm <- b$bench; t <- b$trader
      if (nrow(w) == 0 || nrow(s) == 0) return(div("Waiting."))
      initial <- t$starting_equity_usd
      idx_ok  <- nrow(bm) == 1
      p <- eligible_payout(w$starting_equity_usd, s$equity_adj_usd,
                           if (idx_ok) bm$return_pct else 0,
                           profit_share_pct = t$profit_share_pct,
                           holdback_pct     = t$holdback_pct,
                           initial_capital_usd   = initial,
                           cumulative_equity_usd = b$cum_equity)
      locked        <- isTRUE(p$cumulative_gate_applies) && !p$above_initial_capital
      pending_index <- !idx_ok && !locked
      share_before_gate <- if (p$net_profit_usd > 0 && p$outperformance_usd > 0)
        p$outperformance_usd * p$profit_share_pct / 100 else 0

      rows <- tagList(
        nax_row("Window starting equity", fmt_usd(w$starting_equity_usd)),
        nax_row("Current equity", fmt_usd(s$equity_adj_usd)),
        nax_row("Net profit this window", fmt_usd(p$net_profit_usd)),
        nax_row("Benchmark equivalent", if (idx_ok) fmt_usd(p$benchmark_equiv_usd) else "n/a"),
        nax_row("Outperformance", if (idx_ok) fmt_usd(p$outperformance_usd) else "n/a"),
        nax_row(sprintf("Profit share (%.0f%%), before the inception rule", p$profit_share_pct),
                if (idx_ok) fmt_usd(share_before_gate) else "n/a"),
        nax_row(sprintf("P&L since inception (start %s)", fmt_usd(initial)),
                fmt_usd(p$cumulative_pnl_usd, 0, sign = TRUE))
      )

      total_row <- if (locked) {
        tagList(
          div(class = "nax-payout-row total",
            span(class = "k", "Eligible payout"),
            span(class = "v nax-mono", fmt_usd(0))),
          div(class = "nax-stale", style = "margin-top:10px; margin-bottom:0;",
            sprintf("Locked. A payout needs your equity since inception to be above your starting capital of %s. You are %s below it.",
                    fmt_usd(initial), fmt_usd(p$shortfall_to_initial_usd)))
        )
      } else if (pending_index) {
        tagList(
          div(class = "nax-payout-row total",
            span(class = "k", "Eligible payout"),
            span(class = "v nax-mono", "Pending")),
          div(style = "font-size:12px; color:#64748B; margin-top:8px;",
              "Index data is unavailable, so the payout cannot be indicated yet.")
        )
      } else if (p$holdback_pct > 0) {
        tagList(
          div(class = "nax-payout-row",
            span(class = "k", sprintf("Holdback (%.0f%%)", p$holdback_pct)),
            span(class = "v nax-mono",
                 sprintf("$%s", format(round(p$holdback_usd, 0), big.mark=",")))),
          div(class = "nax-payout-row total",
            span(class = "k", "Payable now"),
            span(class = "v nax-mono",
                 sprintf("$%s", format(round(p$payable_now_usd, 0), big.mark=","))))
        )
      } else {
        div(class = "nax-payout-row total",
          span(class = "k", "Eligible payout"),
          span(class = "v nax-mono",
               sprintf("$%s", format(round(p$eligible_payout_usd, 0), big.mark=","))))
      }

      tagList(rows, total_row)
    })

    # ---------- PERFORMANCE ANALYSIS ----------
    perf_tick <- reactiveTimer(300000)
    perf <- reactive({
      perf_tick()
      tid <- trader_id_rv(); req(tid)
      list(daily = get_equity_daily(tid), mdd = get_max_drawdown_all(tid), trades = get_trades(tid))
    })

    window_scope <- reactive(identical(input$scope, "window") && nrow(bundle()$window) == 1)

    scoped_trades <- reactive({
      tr <- perf()$trades
      if (window_scope()) tr <- tr[tr$closed_at >= bundle()$window$window_start, , drop = FALSE]
      tr
    })

    daily_df <- reactive({
      b <- bundle(); p <- perf()
      d <- daily_pnl(p$daily$day, p$daily$eq_close, b$trader$starting_equity_usd)
      if (window_scope()) d <- d[d$day >= as.Date(b$window$window_start), , drop = FALSE]
      d
    })

    output$kpi_cumulative <- renderUI({
      b <- bundle()
      if (is.na(b$cum_equity)) return(nax_kpi("P&L since inception", "n/a"))
      pnl <- b$cum_equity - b$trader$starting_equity_usd
      nax_kpi("P&L since inception", fmt_usd(pnl, 0, sign = TRUE),
              if (pnl > 0) sprintf("<span class='delta-up'>%s above starting capital</span>", fmt_usd(pnl))
              else sprintf("<span class='delta-down'>%s to regain starting capital</span>", fmt_usd(-pnl)),
              if (pnl > 0) "ok" else "bad")
    })

    output$kpi_maxdd <- renderUI({
      b <- bundle(); t <- b$trader
      mdd <- perf()$mdd
      cur <- if (nrow(b$snap) == 1) b$snap$drawdown_pct %||% 0 else 0
      nax_kpi("Max drawdown (inception)", fmt_pct(mdd),
              sprintf("Now %s · hard stop %.0f%%", fmt_pct(cur), t$hard_stop_pct),
              status_of_pct(mdd / t$hard_stop_pct * 100))
    })

    output$kpi_winrate <- renderUI({
      st <- trade_stats(perf()$trades$pnl_usd)
      nax_kpi("Win rate (inception)", if (st$n == 0) "n/a" else fmt_pct(st$win_rate, 0),
              sprintf("%d wins · %d losses · %d trades", st$wins, st$losses, st$n), "")
    })

    output$kpi_pf <- renderUI({
      st <- trade_stats(perf()$trades$pnl_usd)
      nax_kpi("Profit factor (inception)",
              if (is.na(st$profit_factor)) "n/a" else sprintf("%.2f", st$profit_factor),
              sprintf("Expectancy %s per trade", fmt_usd(st$expectancy, 0, sign = TRUE)),
              if (is.na(st$profit_factor)) "" else if (st$profit_factor >= 1) "ok" else "bad")
    })

    output$daily_plot <- renderPlot({
      d <- daily_df()
      if (nrow(d) == 0) { plot.new(); text(0.5, 0.5, "No daily data yet", col = "#94A3B8"); return(invisible()) }
      op <- par(mar = c(4.5, 4.8, 1, 1), bg = "white", col.axis = "#64748B", col.lab = "#64748B", cex.axis = 0.85)
      on.exit(par(op))
      barplot(d$pnl_usd, col = ifelse(d$pnl_usd >= 0, "#10B981", "#EF4444"), border = NA,
              names.arg = format(d$day, "%d %b"), las = 2, ylab = "Daily P&L (USD)")
      abline(h = 0, col = "#CBD5E1")
    })

    output$daily_stats_ui <- renderUI({
      d <- daily_df(); st <- daily_stats(d)
      if (st$n_days == 0) return(NULL)
      tagList(
        nax_row("Best day", sprintf("%s (%s)", fmt_usd(st$best_usd, 0, TRUE), format(st$best_day, "%d %b"))),
        nax_row("Worst day", sprintf("%s (%s)", fmt_usd(st$worst_usd, 0, TRUE), format(st$worst_day, "%d %b"))),
        nax_row("Average daily return", fmt_pct(st$avg_ret_pct, 2, TRUE)),
        nax_row("Daily volatility (std dev)", fmt_pct(st$sd_ret_pct)),
        nax_row("Profitable days", sprintf("%.0f%% of %d recorded", st$pct_up_days, st$n_days)),
        if (any(diff(d$day) > 1))
          div(style = "font-size:11px; color:#94A3B8; margin-top:8px;",
              "Days with no recorded equity are skipped, so the next recorded day's P&L covers the gap.")
      )
    })

    output$breakdown_ui <- renderUI({
      b <- bundle(); s <- b$snap; w <- b$window; t <- b$trader
      if (nrow(s) == 0) return(div("Waiting."))
      win    <- window_scope()
      start  <- if (win) w$starting_equity_usd else t$starting_equity_usd
      now_eq <- if (win) s$equity_adj_usd else b$cum_equity
      realised <- sum(scoped_trades()$pnl_usd)
      upl      <- s$unrealised_pnl_usd %||% 0
      other    <- (now_eq - start) - realised - upl
      tagList(
        nax_row(if (win) "Equity at window start" else "Starting capital", fmt_usd(start)),
        nax_row("Realised from closed trades", fmt_usd(realised, 0, TRUE)),
        nax_row("Unrealised on open positions", fmt_usd(upl, 0, TRUE)),
        nax_row("Funding, fees and other", fmt_usd(other, 0, TRUE)),
        nax_row(if (win) "Current equity" else "Equity including amounts already swept out",
                fmt_usd(now_eq), "total"),
        div(style = "font-size:11px; color:#94A3B8; margin-top:8px;",
            "Realised P&L is as reported by Bybit for each closed position. 'Other' is the rest of the equity change, mainly funding payments and timing differences.")
      )
    })

    output$trade_stats_ui <- renderUI({
      tr <- scoped_trades(); st <- trade_stats(tr$pnl_usd)
      if (st$n == 0) return(div(style = "color:#64748B;", "No closed trades in this view."))
      fees <- sum(tr$open_fee_usd, tr$close_fee_usd, na.rm = TRUE)
      tagList(
        nax_row("Closed trades", st$n),
        nax_row("Win rate", sprintf("%.0f%% (%d W / %d L)", st$win_rate, st$wins, st$losses)),
        nax_row("Average win", fmt_usd(st$avg_win)),
        nax_row("Average loss", fmt_usd(st$avg_loss)),
        nax_row("Payoff ratio (average win / average loss)",
                if (is.na(st$payoff_ratio)) "n/a" else sprintf("%.2f", st$payoff_ratio)),
        nax_row("Profit factor (gross wins / gross losses)",
                if (is.na(st$profit_factor)) "n/a" else sprintf("%.2f", st$profit_factor)),
        nax_row("Expectancy per trade", fmt_usd(st$expectancy, 0, TRUE)),
        nax_row("Largest win", fmt_usd(st$largest_win)),
        nax_row("Largest loss", fmt_usd(st$largest_loss)),
        nax_row("Total realised", fmt_usd(st$total_pnl, 0, TRUE), "total"),
        nax_row("Trading fees paid (as reported)", fmt_usd(fees))
      )
    })

    sym_df <- reactive({ tr <- scoped_trades(); pnl_by_symbol(tr$symbol, tr$pnl_usd) })

    output$symbol_plot <- renderPlot({
      d <- sym_df()
      if (nrow(d) == 0) { plot.new(); text(0.5, 0.5, "No closed trades in this view", col = "#94A3B8"); return(invisible()) }
      d <- d[order(d$pnl_usd), ]
      op <- par(mar = c(4, 6, 1, 1), bg = "white", col.axis = "#64748B", col.lab = "#64748B", cex.axis = 0.85)
      on.exit(par(op))
      barplot(d$pnl_usd, names.arg = sub("USDT$", "", d$symbol), horiz = TRUE, las = 1,
              col = ifelse(d$pnl_usd >= 0, "#10B981", "#EF4444"), border = NA, xlab = "Realised P&L (USD)")
      abline(v = 0, col = "#CBD5E1")
    })

    output$symbol_table <- renderTable({
      d <- sym_df()
      if (nrow(d) == 0) return(NULL)
      data.frame(Symbol = sub("USDT$", "", d$symbol), Trades = d$trades,
                 `Realised P&L` = fmt_usd(d$pnl_usd, 0, TRUE), check.names = FALSE)
    }, striped = FALSE, bordered = FALSE, spacing = "xs")

    output$recent_trades_table <- renderTable({
      x <- head(scoped_trades(), 15)
      if (nrow(x) == 0) return(NULL)
      data.frame(`Closed (UTC)` = format(x$closed_at, "%d %b %H:%M", tz = "UTC"),
                 Symbol = sub("USDT$", "", x$symbol),
                 Entry = formatC(x$avg_entry, format = "g", digits = 6),
                 Exit = formatC(x$avg_exit, format = "g", digits = 6),
                 `P&L` = fmt_usd(x$pnl_usd, 2, TRUE),
                 Lev = sprintf("%.0fx", x$leverage), check.names = FALSE)
    }, striped = FALSE, bordered = FALSE, spacing = "xs")

    # ---------- POSITIONS ----------
    output$positions_body <- renderUI({
      p <- bundle()$pos
      if (nrow(p) == 0) return(div(style="color:#64748B; font-size:13px;", "No open positions."))
      tableOutput(ns("pos_table"))
    })

    output$pos_table <- renderTable({
      p <- bundle()$pos
      if (nrow(p) == 0) return(NULL)
      data.frame(
        Symbol = p$symbol,
        Side   = p$side,
        Notional = sprintf("$%s", format(round(p$notional_usd, 0), big.mark=",")),
        `UPL` = sprintf("%s$%s",
                       ifelse(p$unrealised_pnl_usd >= 0, "+", "-"),
                       format(abs(round(p$unrealised_pnl_usd, 2)), big.mark=",")),
        Lev = sprintf("%.1fx", p$leverage),
        check.names = FALSE
      )
    }, striped = FALSE, bordered = FALSE, spacing = "xs")

    # ---------- LEDGER ----------
    output$ledger_body <- renderUI({
      t <- bundle()$trader
      pay <- get_payouts(t$trader_id)
      masked_addr <- if (!is.na(t$usdt_address) && nchar(t$usdt_address) >= 6) {
        paste0("…", substring(t$usdt_address, nchar(t$usdt_address) - 5))
      } else "not configured"
      tagList(
        div(style = "font-size: 12px; color:#64748B; margin-bottom: 12px;",
          HTML(sprintf("Invoice to <b>eran@naxcapital.com.au</b> &middot; USDT %s (%s)",
                       masked_addr, t$usdt_network %||% "network tbc"))),
        if (nrow(pay) == 0) {
          div(style="color:#64748B; font-size:13px; padding:4px 0;",
              "No settled windows yet.")
        } else {
          tagList(
            uiOutput(ns("ledger_rows_ui")),
            div(style="font-size:11px; color:#94A3B8; margin-top:10px;",
                "Download the statement to attach to your invoice to Finance.")
          )
        }
      )
    })

    output$ledger_rows_ui <- renderUI({
      t <- bundle()$trader
      pay <- get_payouts(t$trader_id)
      if (nrow(pay) == 0) return(NULL)
      rows <- lapply(seq_len(nrow(pay)), function(i) {
        div(class = "nax-payout-row",
          span(class = "k",
            sprintf("%s → %s",
                    format(pay$window_start[i], "%d %b"),
                    format(pay$window_end[i],   "%d %b"))),
          span(style = "display:flex; gap:12px; align-items:center;",
            span(class = "v nax-mono",
                 sprintf("$%s", format(round(pay$eligible_payout_usd[i], 0), big.mark=","))),
            span(class = sprintf("nax-badge %s",
                                 switch(pay$ledger_status[i],
                                        "paid" = "pass",
                                        "invoiced" = "pending",
                                        "pending")),
                 toupper(pay$ledger_status[i])),
            downloadLink(ns(sprintf("stmt_%d", pay$payout_id[i])),
                         "Statement",
                         style="font-size:11px; color:#0EA5E9;")
          )
        )
      })
      do.call(tagList, rows)
    })

    # Dynamic download handlers per payout
    observe({
      t <- bundle()$trader
      pay <- get_payouts(t$trader_id)
      for (i in seq_len(nrow(pay))) {
        local({
          row <- pay[i, , drop = FALSE]
          trader_row <- t
          output[[sprintf("stmt_%d", row$payout_id)]] <- downloadHandler(
            filename = function() sprintf("nax-statement-%s-%s.html",
                                          row$trader_id,
                                          format(row$window_end, "%Y%m%d")),
            content = function(file) {
              html <- build_statement_html(trader_row, row)
              writeLines(html, file)
            }
          )
        })
      }
    })
  })
}

# ---------- statement HTML generator ----------
build_statement_html <- function(trader, payout) {
  entity <- if (is.na(trader$entity_name)) trader$display_name else trader$entity_name
  usdt_full <- if (is.na(trader$usdt_address)) "(not configured)" else trader$usdt_address
  usdt_net  <- if (is.na(trader$usdt_network)) "(network tbc)" else trader$usdt_network
  issued <- format(Sys.time(), "%d %b %Y %H:%M UTC")
  w_start <- format(payout$window_start, "%d %b %Y")
  w_end   <- format(payout$window_end,   "%d %b %Y")

  paste0(
    "<!doctype html><html><head><meta charset='utf-8'>",
    "<title>NAX Statement - ", entity, " - ", w_end, "</title>",
    "<style>",
    "body{font-family:-apple-system,BlinkMacSystemFont,'Segoe UI',sans-serif;max-width:720px;margin:40px auto;padding:40px;color:#0F172A;}",
    ".brand{font-size:22px;font-weight:700;color:#0F172A;margin-bottom:4px;}",
    ".sub{color:#64748B;font-size:13px;}",
    "h2{margin-top:32px;font-size:16px;color:#0F172A;border-bottom:1px solid #E2E8F0;padding-bottom:8px;}",
    "table{width:100%;border-collapse:collapse;font-size:14px;}",
    "td{padding:8px 4px;}",
    "td.k{color:#64748B;}",
    "td.v{text-align:right;font-family:'JetBrains Mono',ui-monospace,monospace;font-variant-numeric:tabular-nums;}",
    ".total{border-top:2px solid #0F172A;font-weight:600;font-size:16px;}",
    ".notice{background:#FEF3C7;color:#92400E;padding:12px 16px;border-radius:8px;font-size:12px;margin-top:24px;}",
    ".footer{color:#94A3B8;font-size:11px;margin-top:40px;border-top:1px solid #E2E8F0;padding-top:16px;}",
    "</style></head><body>",
    "<div class='brand'>NAX Capital - Statement of amount owed</div>",
    "<div class='sub'>Issued ", issued, "</div>",
    "<h2>Trader</h2>",
    "<table>",
    "<tr><td class='k'>Entity</td><td class='v'>", entity, "</td></tr>",
    "<tr><td class='k'>Trader ID</td><td class='v'>", trader$trader_id, "</td></tr>",
    "<tr><td class='k'>Level</td><td class='v'>", trader$level_code, " ($", format(trader$capital_usd, big.mark=","), ")</td></tr>",
    "</table>",
    "<h2>Window ", w_start, " → ", w_end, "</h2>",
    "<table>",
    "<tr><td class='k'>Starting equity</td><td class='v'>$", format(round(payout$starting_equity_usd,2), big.mark=","), "</td></tr>",
    "<tr><td class='k'>Ending equity</td><td class='v'>$", format(round(payout$ending_equity_usd,2), big.mark=","), "</td></tr>",
    "<tr><td class='k'>Net trading profit</td><td class='v'>$", format(round(payout$net_profit_usd,2), big.mark=","), "</td></tr>",
    "<tr><td class='k'>Index return</td><td class='v'>",
    if (is.na(payout$index_return_pct)) "not recorded" else sprintf("%+.2f%%", payout$index_return_pct), "</td></tr>",
    "<tr><td class='k'>Benchmark-equivalent (floored at $0)</td><td class='v'>$", format(round(payout$benchmark_equiv_usd,2), big.mark=","), "</td></tr>",
    "<tr><td class='k'>Outperformance</td><td class='v'>$", format(round(payout$outperformance_usd,2), big.mark=","), "</td></tr>",
    "<tr><td class='k'>Profit share (", payout$profit_share_pct, "%)</td><td class='v'>$", format(round(payout$eligible_payout_usd,2), big.mark=","), "</td></tr>",
    if (!is.null(payout$holdback_usd) && payout$holdback_usd > 0)
      paste0("<tr><td class='k'>Holdback</td><td class='v'>$", format(round(payout$holdback_usd,2), big.mark=","), "</td></tr>") else "",
    "<tr class='total'><td>Amount owed (USDT)</td><td class='v'>$", format(round(payout$payable_now_usd,2), big.mark=","), "</td></tr>",
    "</table>",
    "<h2>Payment</h2>",
    "<table>",
    "<tr><td class='k'>Invoice to</td><td class='v'>eran@naxcapital.com.au</td></tr>",
    "<tr><td class='k'>USDT address</td><td class='v'>", usdt_full, "</td></tr>",
    "<tr><td class='k'>Network</td><td class='v'>", usdt_net, "</td></tr>",
    "</table>",
    "<div class='notice'>This is a statement of amount owed, not a tax invoice. The trader issues their own invoice to NAX Finance for this amount.</div>",
    "<div class='footer'>Indicative only. Final payout is determined by NAX reconciliation under the Contractor Agreement. ",
    "Generated by NAX Trader Dashboard.</div>",
    "</body></html>"
  )
}
