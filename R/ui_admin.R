# Admin view: overview of all traders + payout ledger editor + audit log.

admin_ui <- function(id = "admin") {
  ns <- NS(id)
  tagList(
    h3("Admin"),
    tabsetPanel(
      tabPanel("Overview", DT::dataTableOutput(ns("overview"))),
      tabPanel("Payout ledger",
        DT::dataTableOutput(ns("ledger")),
        hr(),
        h4("Update ledger row"),
        selectInput(ns("ledger_payout_id"), "Payout", choices = NULL),
        selectInput(ns("ledger_status"), "Status",
                    choices = c("settled", "invoiced", "paid")),
        textInput(ns("ledger_invoice_ref"), "Invoice ref"),
        numericInput(ns("ledger_paid_amount"), "Paid amount (USDT)", value = NA),
        textInput(ns("ledger_tx_id"), "Transaction ID"),
        textAreaInput(ns("ledger_notes"), "Notes"),
        actionButton(ns("ledger_save"), "Save", class = "btn-primary"),
        verbatimTextOutput(ns("ledger_result"))
      ),
      tabPanel("Audit log", DT::dataTableOutput(ns("audit")))
    )
  )
}

admin_server <- function(id, user_id_rv) {
  moduleServer(id, function(input, output, session) {
    tick <- reactiveTimer(60000)

    overview <- reactive({
      tick()
      q("
        select t.trader_id, t.display_name, t.level_code, t.frozen,
               w.window_start, w.window_end,
               s.equity_adj_usd, s.drawdown_pct, s.net_exposure_pct, s.gross_exposure_pct,
               (select count(*) from breaches b where b.trader_id=t.trader_id and b.window_id=w.window_id and b.category='critical') as crit,
               (select count(*) from breaches b where b.trader_id=t.trader_id and b.window_id=w.window_id and b.category='major')    as maj,
               (select count(*) from breaches b where b.trader_id=t.trader_id and b.window_id=w.window_id and b.category='soft')     as soft
        from traders t
        left join lateral (select * from windows where trader_id=t.trader_id order by window_start desc limit 1) w on true
        left join lateral (select * from snapshots where trader_id=t.trader_id order by captured_at desc limit 1) s on true
        order by t.frozen, t.trader_id
      ")
    })

    output$overview <- DT::renderDataTable({
      DT::datatable(overview(), options = list(pageLength = 25), rownames = FALSE)
    })

    ledger <- reactive({
      tick()
      q("
        select p.payout_id, p.trader_id, w.window_start, w.window_end,
               p.eligible_payout_usd, p.payable_now_usd,
               coalesce(l.status,'settled') as status,
               l.invoice_ref, l.invoiced_at, l.paid_amount_usd, l.paid_at, l.paid_tx_id,
               l.notes, l.updated_at, l.updated_by
        from payouts p
        join windows w on p.window_id = w.window_id
        left join payout_ledger l on l.payout_id = p.payout_id
        order by w.window_end desc
      ")
    })

    output$ledger <- DT::renderDataTable({
      DT::datatable(ledger(), options = list(pageLength = 25), rownames = FALSE)
    })

    observe({
      l <- ledger()
      updateSelectInput(session, "ledger_payout_id",
        choices = setNames(l$payout_id, sprintf("%s · %s · $%.2f",
          l$trader_id, format(l$window_end, "%Y-%m-%d"), l$eligible_payout_usd)))
    })

    observeEvent(input$ledger_save, {
      pid <- as.integer(input$ledger_payout_id); if (is.na(pid)) return()
      status <- input$ledger_status
      invoiced_at <- if (status %in% c("invoiced", "paid")) Sys.time() else NA
      paid_at     <- if (status == "paid") Sys.time() else NA
      actor <- user_id_rv() %||% "admin"

      # Snapshot before for audit
      before <- q("select * from payout_ledger where payout_id = $1", pid)

      qe("
        insert into payout_ledger (payout_id, status, invoice_ref, invoiced_at,
                                   paid_amount_usd, paid_at, paid_tx_id, notes,
                                   updated_at, updated_by)
        values ($1,$2,$3,$4,$5,$6,$7,$8, now(), $9)
        on conflict (payout_id) do update set
          status = excluded.status,
          invoice_ref = coalesce(excluded.invoice_ref, payout_ledger.invoice_ref),
          invoiced_at = coalesce(excluded.invoiced_at, payout_ledger.invoiced_at),
          paid_amount_usd = coalesce(excluded.paid_amount_usd, payout_ledger.paid_amount_usd),
          paid_at = coalesce(excluded.paid_at, payout_ledger.paid_at),
          paid_tx_id = coalesce(excluded.paid_tx_id, payout_ledger.paid_tx_id),
          notes = coalesce(excluded.notes, payout_ledger.notes),
          updated_at = now(),
          updated_by = excluded.updated_by
      ",
        pid, status,
        if (nzchar(input$ledger_invoice_ref)) input$ledger_invoice_ref else NA,
        invoiced_at,
        if (!is.na(input$ledger_paid_amount)) input$ledger_paid_amount else NA,
        paid_at,
        if (nzchar(input$ledger_tx_id)) input$ledger_tx_id else NA,
        if (nzchar(input$ledger_notes)) input$ledger_notes else NA,
        actor)

      qe("
        insert into admin_audit (actor, action, target_table, target_id, before_json, note)
        values ($1, 'update_ledger', 'payout_ledger', $2, $3::jsonb, 'ledger update')
      ", actor, as.character(pid), jsonlite::toJSON(before, auto_unbox = TRUE))

      output$ledger_result <- renderText(sprintf("Saved payout_id=%d at %s", pid, Sys.time()))
    })

    output$audit <- DT::renderDataTable({
      tick()
      DT::datatable(
        q("select occurred_at, actor, action, target_table, target_id, note
           from admin_audit order by occurred_at desc limit 500"),
        options = list(pageLength = 25), rownames = FALSE
      )
    })
  })
}
