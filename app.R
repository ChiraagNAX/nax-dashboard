# NAX Trader Dashboard
# shinyapps.io: k95y82-chiraag-nax/nax-dashboard
#
# Auth: shinymanager with bcrypt passwords in the users table.
# After login, session$userData$user_id and $trader_id are set.

suppressPackageStartupMessages({
  library(shiny)
  library(shinymanager)
  library(DT)
  library(bcrypt)
  library(jsonlite)
})

source("R/calc.R")
source("R/db.R")
source("R/ui_trader.R")
source("R/ui_admin.R")

.auth_log <- function(msg) {
  prev <- getOption("nax.auth.log", character(0))
  line <- sprintf("[%s] %s", format(Sys.time()), msg)
  options(nax.auth.log = c(tail(prev, 20), line))
}

# ---- auth adapter — called by shinymanager as check_credentials(user, password) ----
nax_check_credentials <- function(user, password) {
  .auth_log(sprintf("ATTEMPT user='%s' pwdlen=%d", as.character(user), nchar(password)))
  u <- tryCatch(get_user(user), error = function(e) {
    .auth_log(sprintf("get_user ERR: %s", conditionMessage(e))); NULL
  })
  if (is.null(u) || nrow(u) == 0) {
    .auth_log(sprintf("no row for '%s'", user)); return(list(result = FALSE))
  }
  ok <- tryCatch(bcrypt::checkpw(password, u$password_hash),
                 error = function(e) { .auth_log(sprintf("bcrypt ERR: %s", conditionMessage(e))); FALSE })
  .auth_log(sprintf("bcrypt=%s for '%s'", as.character(ok), user))
  if (!ok) return(list(result = FALSE))
  tryCatch(touch_login(user), error = function(e) NULL)
  list(result = TRUE,
       user_info = list(user = as.character(user), role = u$role, trader_id = u$trader_id))
}

.diag_db <- function() {
  lines <- getOption("nax.auth.log", character(0))
  n <- length(lines)
  if (n == 0) return("AUTH LOG: (empty)")
  sprintf("AUTH LOG (%d): %s", n, paste(lines, collapse = " || "))
}

ui <- fluidPage(
  tags$head(tags$title("NAX Trader Dashboard"),
            tags$style(HTML("
              body { font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif; }
              .well { background: #fafafa; }
              h3 { color: #2c3e50; }
            "))),
  uiOutput("page")
)

ui <- secure_app(ui,
                 tags_top = tags$div(
                   tags$h3("NAX Trader Dashboard"),
                   tags$pre(style = "font-size:10px; color:#666; white-space:pre-wrap;",
                            tryCatch(.diag_db(), error = function(e) sprintf("diag error: %s", conditionMessage(e))))
                 ),
                 enable_admin = FALSE)

server <- function(input, output, session) {
  auth <- secure_server(check_credentials = nax_check_credentials)

  user_id_rv   <- reactive({ auth$user })
  trader_id_rv <- reactive({
    info <- auth$user_info
    if (is.null(info)) NULL else info$trader_id
  })
  role_rv <- reactive({
    info <- auth$user_info
    if (is.null(info)) NULL else info$role
  })

  # Start both module servers unconditionally; UI switch decides what shows.
  trader_server("trader", trader_id_rv)
  admin_server("admin",   user_id_rv)

  output$page <- renderUI({
    req(auth$user, role_rv())
    if (identical(role_rv(), "admin")) {
      admin_ui("admin")
    } else {
      req(trader_id_rv())
      trader_ui("trader")
    }
  })
}

shinyApp(ui, server)
