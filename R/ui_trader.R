# Trader view — bslib layout, status-aware colors, financial dashboard feel

trader_ui <- function(id = "trader") {
  ns <- NS(id)
  tagList(
    uiOutput(ns("header")),
    uiOutput(ns("staleness_banner")),

    # Row 1: KPI strip
    fluidRow(
      column(3, uiOutput(ns("kpi_equity"))),
      column(3, uiOutput(ns("kpi_return"))),
      column(3, uiOutput(ns("kpi_drawdown"))),
      column(3, uiOutput(ns("kpi_vs_index")))
    ),
    br(),

    # Row 2: Equity chart (wide) + Limits (narrow)
    fluidRow(
      column(8,
        card(
          card_header("Equity vs HWM"),
          card_body(plotOutput(ns("dd_plot"), height = "280px"))
        )
      ),
      column(4,
        card(
          card_header("Risk limits"),
          card_body(uiOutput(ns("limits_body")))
        )
      )
    ),
    br(),

    # Row 3: Breaches + Benchmark + Payout working
    fluidRow(
      column(4,
        card(
          card_header("Breaches (this window)"),
          card_body(uiOutput(ns("breaches_body")))
        )
      ),
      column(4,
        card(
          card_header("Benchmark vs NAX Index"),
          card_body(uiOutput(ns("benchmark_body")))
        )
      ),
      column(4,
        card(
          card_header("Indicative payout"),
          card_body(uiOutput(ns("payout_body")))
        )
      )
    ),
    br(),

    # Positions + Ledger
    fluidRow(
      column(6,
        card(
          card_header("Open positions"),
          card_body(uiOutput(ns("positions_body")))
        )
      ),
      column(6,
        card(
          card_header("Payout ledger"),
          card_body(uiOutput(ns("ledger_body")))
        )
      )
    ),

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
      list(trader = trader, window = window, snap = snap, bench = bench,
           breaches = breaches, pos = pos, series = series)
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

      div(class = "nax-header",
        tags$h2(t$display_name, lvl_badge),
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
      if (nrow(s) == 0) return(nax_kpi("Equity", "—"))
      nax_kpi("Equity (USD)",
              sprintf("$%s", format(round(s$equity_adj_usd, 0), big.mark = ",")),
              sprintf("Unrealised %s$%s",
                      if ((s$unrealised_pnl_usd %||% 0) >= 0) "+" else "",
                      format(round(s$unrealised_pnl_usd %||% 0, 0), big.mark = ",")))
    })

    output$kpi_return <- renderUI({
      b <- bundle(); s <- b$snap; w <- b$window
      if (nrow(s) == 0 || nrow(w) == 0) return(nax_kpi("Window return", "—"))
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
      if (nrow(s) == 0) return(nax_kpi("Drawdown", "—"))
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
      if (nrow(s) == 0 || nrow(w) == 0) return(nax_kpi("vs Index", "—"))
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
               if (is.na(idx_pct)) "—" else sprintf("%+.2f%%", idx_pct))),
        div(class = "nax-payout-row",
          span(class = "k", "Difference"),
          span(class = "v nax-mono",
               if (is.na(diff)) "—" else sprintf("%+.2f pp", diff))),
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
      idx_pct <- if (nrow(bm) == 1) bm$return_pct else 0
      p <- eligible_payout(w$starting_equity_usd, s$equity_adj_usd, idx_pct,
                           profit_share_pct = t$profit_share_pct,
                           holdback_pct     = t$holdback_pct)

      rows <- tagList(
        div(class = "nax-payout-row",
          span(class = "k", "Starting equity"),
          span(class = "v nax-mono", sprintf("$%s", format(round(w$starting_equity_usd, 0), big.mark=",")))),
        div(class = "nax-payout-row",
          span(class = "k", "Current equity"),
          span(class = "v nax-mono", sprintf("$%s", format(round(s$equity_adj_usd, 0), big.mark=",")))),
        div(class = "nax-payout-row",
          span(class = "k", "Net profit"),
          span(class = "v nax-mono",
               sprintf("%s$%s",
                       if (p$net_profit_usd >= 0) "" else "-",
                       format(abs(round(p$net_profit_usd, 0)), big.mark=",")))),
        div(class = "nax-payout-row",
          span(class = "k", "Benchmark equivalent"),
          span(class = "v nax-mono",
               sprintf("$%s", format(round(p$benchmark_equiv_usd, 0), big.mark=",")))),
        div(class = "nax-payout-row",
          span(class = "k", "Outperformance"),
          span(class = "v nax-mono",
               sprintf("%s$%s",
                       if (p$outperformance_usd >= 0) "" else "-",
                       format(abs(round(p$outperformance_usd, 0)), big.mark=",")))),
        div(class = "nax-payout-row",
          span(class = "k", sprintf("Profit share (%.0f%%)", p$profit_share_pct)),
          span(class = "v nax-mono",
               sprintf("$%s", format(round(p$eligible_payout_usd, 0), big.mark=","))))
      )

      total_row <- if (p$holdback_pct > 0) {
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
    "<title>NAX Statement — ", entity, " — ", w_end, "</title>",
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
    "<div class='brand'>NAX Capital — Statement of amount owed</div>",
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
    "<tr><td class='k'>Index return</td><td class='v'>", sprintf("%+.2f%%", payout$index_return_pct), "</td></tr>",
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
