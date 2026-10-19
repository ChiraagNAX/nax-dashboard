# Trader view: header, account, limits, drawdown, breaches, benchmark, payout.

trader_ui <- function(id = "trader") {
  ns <- NS(id)
  tagList(
    uiOutput(ns("header")),
    uiOutput(ns("staleness_banner")),

    fluidRow(
      column(6, uiOutput(ns("account_card"))),
      column(6, uiOutput(ns("limits_card")))
    ),

    fluidRow(
      column(6, uiOutput(ns("drawdown_card")),
                plotOutput(ns("dd_plot"), height = "260px")),
      column(6, uiOutput(ns("breaches_card")))
    ),

    fluidRow(
      column(6, uiOutput(ns("benchmark_card"))),
      column(6, uiOutput(ns("payout_card")))
    ),

    fluidRow(
      column(12, uiOutput(ns("ledger_card")))
    ),

    hr(),
    div(style = "font-size: 11px; color: #666;",
        "Indicative only. Payouts are determined by NAX reconciliation under your Contractor Agreement."),
    div(style = "font-size: 11px; color: #666;",
        a("Programme document", href = "#"), " · ",
        a("Index methodology", href = "https://k95y82-chiraag-nax.shinyapps.io/nax-crypto-index", target = "_blank"), " · ",
        a("How this is calculated", href = "#"))
  )
}

trader_server <- function(id, trader_id_rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Reactive polling: refresh every 30s
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
      list(trader = trader, window = window, snap = snap, bench = bench,
           breaches = breaches, pos = pos, series = series)
    })

    output$header <- renderUI({
      b <- bundle()
      t <- b$trader; w <- b$window; s <- b$snap
      if (nrow(t) == 0) return(div("Trader not found."))
      hdr <- h3(sprintf("%s — %s (%s, $%s)",
                        t$display_name, t$level_name %||% t$level_code, t$level_code,
                        format(t$capital_usd, big.mark = ",")))
      sub <- if (nrow(w) == 1) {
        days_elapsed <- as.numeric(difftime(Sys.time(), w$window_start, units = "days"))
        days_total   <- as.numeric(difftime(w$window_end, w$window_start, units = "days"))
        days_left    <- max(0, days_total - days_elapsed)
        p(sprintf("Window %s → %s UTC · day %.1f / %.0f · %.1f days left",
                  format(w$window_start, "%Y-%m-%d %H:%M"),
                  format(w$window_end,   "%Y-%m-%d %H:%M"),
                  days_elapsed, days_total, days_left))
      } else p("No active window.")
      stamp <- if (nrow(s) == 1) p(sprintf("Last updated: %s UTC",
                                           format(s$captured_at, "%Y-%m-%d %H:%M:%S", tz = "UTC")))
               else p("No snapshot yet.")
      tagList(hdr, sub, stamp)
    })

    output$staleness_banner <- renderUI({
      s <- bundle()$snap
      if (nrow(s) == 0) return(NULL)
      age_min <- as.numeric(difftime(Sys.time(), s$captured_at, units = "mins"))
      if (age_min > 20) {
        div(class = "alert alert-warning",
            sprintf("Data is %.0f minutes old. Values may not reflect the live account.", age_min))
      }
    })

    output$account_card <- renderUI({
      b <- bundle(); s <- b$snap; w <- b$window
      if (nrow(s) == 0) return(wellPanel(h4("Account"), p("Waiting for first snapshot.")))
      start_eq <- if (nrow(w) == 1) w$starting_equity_usd else b$trader$starting_equity_usd
      ret_usd  <- s$equity_adj_usd - start_eq
      ret_pct  <- ret_usd / start_eq * 100
      wellPanel(
        h4("Account"),
        tags$table(class = "table table-sm",
          tags$tr(tags$td("Equity (adjusted)"),      tags$td(sprintf("$%.2f", s$equity_adj_usd))),
          tags$tr(tags$td("Equity (raw)"),           tags$td(sprintf("$%.2f", s$equity_usd))),
          tags$tr(tags$td("Window return"),          tags$td(sprintf("$%.2f (%+.2f%%)", ret_usd, ret_pct))),
          tags$tr(tags$td("Unrealised P&L"),         tags$td(sprintf("$%.2f", s$unrealised_pnl_usd %||% 0))),
          tags$tr(tags$td("Open positions"),         tags$td(s$position_count %||% 0))
        ),
        if (nrow(b$pos) > 0) {
          tagList(h5("Positions"),
            tableOutput(ns("pos_table")))
        }
      )
    })

    output$pos_table <- renderTable({
      p <- bundle()$pos
      if (nrow(p) == 0) return(NULL)
      data.frame(
        Symbol   = p$symbol,
        Side     = p$side,
        Notional = sprintf("$%.0f", p$notional_usd),
        UPL      = sprintf("$%.2f", p$unrealised_pnl_usd),
        Lev      = sprintf("%.1fx", p$leverage)
      )
    }, striped = TRUE)

    output$limits_card <- renderUI({
      b <- bundle(); s <- b$snap; t <- b$trader
      if (nrow(s) == 0) return(wellPanel(h4("Limits"), p("Waiting.")))
      gauge <- function(label, value, limit, unit = "%") {
        pct_of_limit <- if (limit > 0) abs(value) / limit * 100 else 0
        color <- if (pct_of_limit >= 100) "#c0392b" else if (pct_of_limit >= 75) "#e67e22" else "#27ae60"
        div(style = "margin-bottom: 10px;",
          tags$strong(label), tags$span(style = "float:right;",
            sprintf("%.1f%s / %.1f%s", value, unit, limit, unit)),
          div(style = sprintf("height:8px; background:#eee; border-radius:4px; margin-top:4px;"),
            div(style = sprintf("height:8px; width:%.0f%%; background:%s; border-radius:4px;",
                                min(100, pct_of_limit), color))
          )
        )
      }
      wellPanel(
        h4("Limits"),
        gauge("Net exposure",   s$net_exposure_pct   %||% 0, t$max_net_pct),
        gauge("Gross exposure", s$gross_exposure_pct %||% 0, t$max_gross_pct),
        gauge("Max per-symbol leverage", s$max_leverage %||% 0, t$max_leverage, "x"),
        gauge("Drawdown", s$drawdown_pct %||% 0, t$hard_stop_pct)
      )
    })

    output$drawdown_card <- renderUI({
      b <- bundle(); s <- b$snap; t <- b$trader
      if (nrow(s) == 0) return(wellPanel(h4("Drawdown"), p("Waiting.")))
      hwm <- s$hwm_usd
      soft_level <- hwm * (1 - t$soft_dd_pct   / 100)
      hard_level <- hwm * (1 - t$hard_stop_pct / 100)
      wellPanel(
        h4("Drawdown reference"),
        tags$table(class = "table table-sm",
          tags$tr(tags$td("Current HWM"),     tags$td(sprintf("$%.2f", hwm))),
          tags$tr(tags$td("Current equity"),  tags$td(sprintf("$%.2f", s$equity_adj_usd))),
          tags$tr(tags$td("Current drawdown"),tags$td(sprintf("%.2f%%", s$drawdown_pct %||% 0))),
          tags$tr(tags$td(sprintf("Soft (%.0f%%)", t$soft_dd_pct)),
                  tags$td(sprintf("$%.2f", soft_level))),
          tags$tr(tags$td(sprintf("Hard stop (%.0f%%)", t$hard_stop_pct)),
                  tags$td(sprintf("$%.2f", hard_level)))
        )
      )
    })

    output$dd_plot <- renderPlot({
      b <- bundle(); s <- b$series; t <- b$trader
      if (nrow(s) == 0) return(invisible())
      hwm_now <- max(s$hwm_usd, na.rm = TRUE)
      soft <- hwm_now * (1 - t$soft_dd_pct   / 100)
      hard <- hwm_now * (1 - t$hard_stop_pct / 100)
      op <- par(mar = c(3, 4, 1, 1)); on.exit(par(op))
      plot(s$captured_at, s$equity_adj_usd, type = "l", lwd = 2, col = "#2c3e50",
           xlab = "", ylab = "USD", ylim = range(c(s$equity_adj_usd, hard, s$hwm_usd), na.rm = TRUE))
      lines(s$captured_at, s$hwm_usd, lty = 2, col = "#27ae60")
      abline(h = soft, col = "#e67e22", lty = 3)
      abline(h = hard, col = "#c0392b", lty = 3)
      legend("bottomleft", bty = "n", lty = c(1,2,3,3), lwd = c(2,1,1,1),
             col = c("#2c3e50", "#27ae60", "#e67e22", "#c0392b"),
             legend = c("Equity", "HWM", "Soft", "Hard"))
    })

    output$breaches_card <- renderUI({
      b <- bundle(); br <- b$breaches
      c_count <- sum(br$category == "critical")
      m_count <- sum(br$category == "major")
      s_count <- sum(br$category == "soft")
      wellPanel(
        h4("Breach record (this window)"),
        tags$table(class = "table table-sm",
          tags$tr(tags$td("Critical"), tags$td(sprintf("%d / 0", c_count))),
          tags$tr(tags$td("Major"),    tags$td(sprintf("%d / 1", m_count))),
          tags$tr(tags$td("Soft DD"),  tags$td(sprintf("%d / 1", s_count)))
        ),
        if (nrow(br) > 0) {
          tagList(h5("Events"), tableOutput(ns("breach_table")))
        } else p("No breaches this window."),
        tags$small(
          tags$b("Critical"), ": hard stop, max-leverage, outside universe, unauthorised use. Window-ending.", tags$br(),
          tags$b("Major"),    ": net or gross exposure over limit. Allowance 1.", tags$br(),
          tags$b("Soft"),     ": drawdown past the soft threshold. Allowance 1."
        )
      )
    })

    output$breach_table <- renderTable({
      br <- bundle()$breaches
      if (nrow(br) == 0) return(NULL)
      data.frame(
        When     = format(br$occurred_at, "%Y-%m-%d %H:%M UTC"),
        Category = br$category,
        Kind     = br$kind,
        Observed = br$observed_value,
        Limit    = br$limit_value
      )
    }, striped = TRUE)

    output$benchmark_card <- renderUI({
      b <- bundle(); w <- b$window; s <- b$snap; bm <- b$bench
      if (nrow(w) == 0 || nrow(s) == 0) return(wellPanel(h4("Benchmark"), p("Waiting.")))
      start_eq <- w$starting_equity_usd
      ret_pct  <- (s$equity_adj_usd - start_eq) / start_eq * 100
      idx_pct  <- if (nrow(bm) == 1) bm$return_pct else NA
      diff     <- if (!is.na(idx_pct)) ret_pct - idx_pct else NA
      v <- verdict(ret_pct, idx_pct %||% NA_real_,
                   sum(b$breaches$category == "critical"),
                   sum(b$breaches$category == "major"),
                   sum(b$breaches$category == "soft"))
      wellPanel(
        h4("Benchmark (NAX Crypto Index)"),
        tags$table(class = "table table-sm",
          tags$tr(tags$td("Trader return"), tags$td(sprintf("%+.2f%%", ret_pct))),
          tags$tr(tags$td("Index return"),  tags$td(if (is.na(idx_pct)) "n/a" else sprintf("%+.2f%%", idx_pct))),
          tags$tr(tags$td("Difference"),    tags$td(if (is.na(diff)) "n/a" else sprintf("%+.2f pp", diff))),
          tags$tr(tags$td("Verdict track"), tags$td(tags$strong(v$status), " ", v$reason))
        )
      )
    })

    output$payout_card <- renderUI({
      b <- bundle(); w <- b$window; s <- b$snap; bm <- b$bench; t <- b$trader
      if (nrow(w) == 0 || nrow(s) == 0) return(wellPanel(h4("Payout"), p("Waiting.")))
      idx_pct <- if (nrow(bm) == 1) bm$return_pct else 0
      p <- eligible_payout(w$starting_equity_usd, s$equity_adj_usd, idx_pct,
                           profit_share_pct = t$profit_share_pct,
                           holdback_pct     = t$holdback_pct)
      wellPanel(
        h4("Indicative payout (current window)"),
        tags$table(class = "table table-sm",
          tags$tr(tags$td("Starting equity"),       tags$td(sprintf("$%.2f", w$starting_equity_usd))),
          tags$tr(tags$td("Ending equity"),         tags$td(sprintf("$%.2f", s$equity_adj_usd))),
          tags$tr(tags$td("Net trading profit"),    tags$td(sprintf("$%.2f", p$net_profit_usd))),
          tags$tr(tags$td("Index return"),          tags$td(sprintf("%+.2f%%", idx_pct))),
          tags$tr(tags$td("Benchmark-equivalent (floored at 0)"),
                                                     tags$td(sprintf("$%.2f", p$benchmark_equiv_usd))),
          tags$tr(tags$td("Outperformance"),        tags$td(sprintf("$%.2f", p$outperformance_usd))),
          tags$tr(tags$td(sprintf("Profit share (%.0f%%)", p$profit_share_pct)),
                                                     tags$td(sprintf("$%.2f", p$eligible_payout_usd))),
          if (p$holdback_pct > 0) {
            tagList(
              tags$tr(tags$td(sprintf("Holdback (%.0f%%)", p$holdback_pct)),
                      tags$td(sprintf("$%.2f", p$holdback_usd))),
              tags$tr(tags$td(tags$strong("Payable now")),
                      tags$td(tags$strong(sprintf("$%.2f", p$payable_now_usd))))
            )
          } else {
            tags$tr(tags$td(tags$strong("Eligible payout")),
                    tags$td(tags$strong(sprintf("$%.2f", p$eligible_payout_usd))))
          }
        )
      )
    })

    output$ledger_card <- renderUI({
      t <- bundle()$trader
      pay <- get_payouts(t$trader_id)
      masked_addr <- if (!is.na(t$usdt_address) && nchar(t$usdt_address) >= 6) {
        paste0("…", substring(t$usdt_address, nchar(t$usdt_address) - 5))
      } else "not configured"
      wellPanel(
        h4("Payout ledger"),
        p(sprintf("Invoice to: eran@naxcapital.com.au · USDT address on file: %s (%s)",
                  masked_addr, t$usdt_network %||% "network tbc")),
        if (nrow(pay) == 0) p("No settled windows yet.")
        else {
          tableOutput(ns("ledger_table"))
        }
      )
    })

    output$ledger_table <- renderTable({
      t <- bundle()$trader
      pay <- get_payouts(t$trader_id)
      if (nrow(pay) == 0) return(NULL)
      data.frame(
        Window   = sprintf("%s → %s",
                           format(pay$window_start, "%Y-%m-%d"),
                           format(pay$window_end,   "%Y-%m-%d")),
        Payout   = sprintf("$%.2f", pay$eligible_payout_usd),
        Status   = pay$ledger_status,
        Invoiced = ifelse(is.na(pay$invoiced_at), "", format(pay$invoiced_at, "%Y-%m-%d")),
        Paid     = ifelse(is.na(pay$paid_at),     "", format(pay$paid_at,     "%Y-%m-%d")),
        TxId     = ifelse(is.na(pay$paid_tx_id),  "", pay$paid_tx_id)
      )
    }, striped = TRUE)
  })
}

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || (length(a) == 1 && is.na(a))) b else a
