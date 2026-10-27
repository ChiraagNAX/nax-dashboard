# Admin view: overview, ledger editor, transfer logger, password reset, audit log

admin_ui <- function(id = "admin") {
  ns <- NS(id)
  tagList(
    h3("Admin console", style = "color:#0F172A; font-weight:600;"),
    tabsetPanel(
      tabPanel("Overview",
        br(), DT::dataTableOutput(ns("overview"))),
      tabPanel("Payout ledger",
        br(),
        DT::dataTableOutput(ns("ledger")),
        hr(),
        h5("Update ledger row"),
        fluidRow(
          column(4, selectInput(ns("ledger_payout_id"), "Payout", choices = NULL)),
          column(4, selectInput(ns("ledger_status"), "Status",
                                choices = c("settled", "invoiced", "paid"))),
          column(4, textInput(ns("ledger_invoice_ref"), "Invoice ref"))
        ),
        fluidRow(
          column(4, numericInput(ns("ledger_paid_amount"), "Paid amount (USDT)", value = NA)),
          column(8, textInput(ns("ledger_tx_id"), "Transaction ID"))
        ),
        textAreaInput(ns("ledger_notes"), "Notes", width = "100%", rows = 2),
        actionButton(ns("ledger_save"), "Save ledger", class = "btn-primary"),
        verbatimTextOutput(ns("ledger_result"))
      ),
      tabPanel("Transfers",
        br(),
        p("Log deposits, withdrawals and settlement sweeps so they are not counted as trading losses. Use kind 'settlement' with a negative amount for the sweep to the reconciliation account: logging one also resets that trader's high-water mark to the balance after the sweep. Without a logged settlement the high-water mark keeps trailing up and is never reset by a loss.", style="color:#64748B;"),
        fluidRow(
          column(3, selectInput(ns("xfer_trader"), "Trader", choices = NULL)),
          column(2, selectInput(ns("xfer_kind"), "Kind",
                                choices = c("settlement", "deposit", "withdrawal", "internal"))),
          column(3, numericInput(ns("xfer_amount"), "Amount USD (sign: deposit +, withdrawal −)", value = 0)),
          column(4, textInput(ns("xfer_note"), "Note"))
        ),
        actionButton(ns("xfer_save"), "Log transfer", class = "btn-primary"),
        verbatimTextOutput(ns("xfer_result")),
        hr(),
        h5("Recent transfers"),
        DT::dataTableOutput(ns("xfer_table"))
      ),
      tabPanel("Users",
        br(),
        p("Reset a trader's password. New random password is shown once — copy and send securely.", style = "color:#64748B;"),
        fluidRow(
          column(4, selectInput(ns("pw_user"), "User", choices = NULL)),
          column(4, actionButton(ns("pw_reset"), "Generate new password", class = "btn-primary"))
        ),
        verbatimTextOutput(ns("pw_result"))
      ),
      tabPanel("Audit log",
        br(), DT::dataTableOutput(ns("audit")))
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
        choices = setNames(l$payout_id,
          sprintf("%s · %s · $%.2f", l$trader_id,
                  format(l$window_end, "%Y-%m-%d"), l$eligible_payout_usd)))
    })

    observeEvent(input$ledger_save, {
      pid <- as.integer(input$ledger_payout_id); if (is.na(pid)) return()
      status <- input$ledger_status
      invoiced_at <- if (status %in% c("invoiced", "paid")) Sys.time() else NA
      paid_at     <- if (status == "paid") Sys.time() else NA
      actor <- user_id_rv() %||% "admin"
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

    # ---- Transfers ----
    traders_list <- reactive({
      tick()
      q("select trader_id, display_name from traders where frozen = false order by display_name")
    })

    observe({
      t <- traders_list()
      updateSelectInput(session, "xfer_trader",
        choices = setNames(t$trader_id, t$display_name))
    })

    output$xfer_table <- DT::renderDataTable({
      tick()
      DT::datatable(
        q("select t.transfer_id, t.trader_id, t.occurred_at, t.kind, t.amount_usd, t.note, t.source
           from transfers t order by t.occurred_at desc limit 200"),
        options = list(pageLength = 15), rownames = FALSE)
    })

    observeEvent(input$xfer_save, {
      tid <- input$xfer_trader; if (!nzchar(tid)) return()
      amt <- as.numeric(input$xfer_amount)
      if (is.na(amt) || amt == 0) {
        output$xfer_result <- renderText("Amount cannot be zero.")
        return()
      }
      actor <- user_id_rv() %||% "admin"
      qe("insert into transfers (trader_id, occurred_at, amount_usd, kind, note, source)
          values ($1, now(), $2, $3, $4, 'manual')",
         tid, amt, input$xfer_kind,
         if (nzchar(input$xfer_note)) input$xfer_note else NA)
      qe("insert into admin_audit (actor, action, target_table, target_id, note)
          values ($1, 'log_transfer', 'transfers', $2, $3)",
         actor, tid, sprintf("%s $%.2f (%s)", input$xfer_kind, amt, input$xfer_note))
      output$xfer_result <- renderText(
        sprintf("Logged %s of $%.2f for %s at %s", input$xfer_kind, amt, tid, Sys.time()))
    })

    # ---- Password reset ----
    all_users <- reactive({
      tick()
      q("select user_id, role from users order by user_id")
    })
    observe({
      u <- all_users()
      updateSelectInput(session, "pw_user",
        choices = setNames(u$user_id, sprintf("%s (%s)", u$user_id, u$role)))
    })

    observeEvent(input$pw_reset, {
      uid <- input$pw_user; if (!nzchar(uid)) return()
      new_pw <- paste0("nax-", paste(sample(c(0:9, letters), 10, replace=TRUE), collapse=""))
      h <- bcrypt::hashpw(new_pw)
      qe("update users set password_hash = $1 where user_id = $2", h, uid)
      actor <- user_id_rv() %||% "admin"
      qe("insert into admin_audit (actor, action, target_table, target_id, note)
          values ($1, 'reset_password', 'users', $2, 'password reset')",
         actor, uid)
      output$pw_result <- renderText(sprintf(
        "New password for %s: %s\n\nShow this to the user once — it will not be stored or shown again.",
        uid, new_pw))
    })

    # ---- Audit log ----
    output$audit <- DT::renderDataTable({
      tick()
      DT::datatable(
        q("select occurred_at, actor, action, target_table, target_id, note
           from admin_audit order by occurred_at desc limit 500"),
        options = list(pageLength = 25), rownames = FALSE)
    })
  })
}

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || (length(a) == 1 && is.na(a))) b else a
