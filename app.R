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

.auth_log_path <- file.path(tempdir(), "nax-auth.log")

.auth_log <- function(msg) {
  tryCatch(writeLines(sprintf("[%s] %s", format(Sys.time()), msg),
                      con = .auth_log_path, sep = "\n"), error = function(e) NULL)
  tryCatch(cat(sprintf("[%s] %s\n", format(Sys.time()), msg),
               file = .auth_log_path, append = TRUE), error = function(e) NULL)
  message(msg)
}

# ---- auth adapter ----
nax_check_credentials <- function() {
  function(user, password) {
    .auth_log(sprintf("ATTEMPT user='%s' pwdlen=%d pwdhead='%s'",
                      as.character(user), nchar(password), substr(password, 1, 4)))
    u <- tryCatch(get_user(user), error = function(e) {
      .auth_log(sprintf("get_user ERR: %s", conditionMessage(e))); NULL
    })
    if (is.null(u) || nrow(u) == 0) {
      .auth_log(sprintf("no row for '%s'", user)); return(list(result = FALSE))
    }
    .auth_log(sprintf("found row for '%s', hash head='%s'", user, substr(u$password_hash, 1, 7)))
    ok <- tryCatch(bcrypt::checkpw(password, u$password_hash),
                   error = function(e) { .auth_log(sprintf("bcrypt ERR: %s", conditionMessage(e))); FALSE })
    .auth_log(sprintf("bcrypt result=%s", as.character(ok)))
    if (!ok) return(list(result = FALSE))
    tryCatch(touch_login(user), error = function(e) NULL)
    list(result = TRUE,
         user_info = list(user = user, role = u$role, trader_id = u$trader_id))
  }
}

.diag_db <- function() {
  log_tail <- tryCatch({
    if (file.exists(.auth_log_path)) {
      ls <- readLines(.auth_log_path, warn = FALSE)
      paste(tail(ls, 10), collapse = " || ")
    } else "no log yet"
  }, error = function(e) sprintf("log err: %s", conditionMessage(e)))
  sprintf("AUTH LOG: %s", log_tail)
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
  auth <- secure_server(check_credentials = nax_check_credentials())

  user_id_rv   <- reactive({ auth$user })
  trader_id_rv <- reactive({
    info <- auth$user_info
    if (is.null(info)) NULL else info$trader_id
  })
  role_rv <- reactive({
    info <- auth$user_info
    if (is.null(info)) "trader" else info$role
  })

  output$page <- renderUI({
    req(auth$user)
    if (role_rv() == "admin") {
      admin_ui("admin")
    } else {
      req(trader_id_rv())
      trader_ui("trader")
    }
  })

  observe({
    if (!is.null(auth$user) && role_rv() == "admin") {
      admin_server("admin", user_id_rv)
    }
  })

  observe({
    tid <- trader_id_rv()
    if (!is.null(auth$user) && !is.null(tid) && role_rv() != "admin") {
      trader_server("trader", trader_id_rv)
    }
  })
}

shinyApp(ui, server)
