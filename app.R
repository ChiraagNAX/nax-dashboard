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

# ---- auth adapter: shinymanager calls check_credentials() ----
nax_check_credentials <- function() {
  function(user, password) {
    u <- tryCatch(get_user(user), error = function(e) {
      message(sprintf("[AUTH] get_user error for %s: %s", user, conditionMessage(e)))
      NULL
    })
    if (is.null(u) || nrow(u) == 0) {
      message(sprintf("[AUTH] no row for user=%s (db host=%s)",
                      user, Sys.getenv("NAX_PG_HOST", "unset")))
      return(list(result = FALSE))
    }
    ok <- tryCatch(bcrypt::checkpw(password, u$password_hash),
                   error = function(e) { message("[AUTH] bcrypt err: ", conditionMessage(e)); FALSE })
    if (!ok) {
      message(sprintf("[AUTH] bcrypt mismatch user=%s", user))
      return(list(result = FALSE))
    }
    tryCatch(touch_login(user), error = function(e) NULL)
    message(sprintf("[AUTH] success user=%s role=%s trader=%s", user, u$role, u$trader_id))
    list(result = TRUE,
         user_info = list(user = user, role = u$role, trader_id = u$trader_id))
  }
}

.diag_db <- function() {
  res <- tryCatch({
    all_users <- q("select user_id, role, trader_id from users")
    roja <- tryCatch(get_user("roja"), error = function(e) sprintf("get_user err: %s", conditionMessage(e)))
    sprintf("users=%d all=%s | get_user(roja) rows=%s cols=%s",
      nrow(all_users),
      paste(all_users$user_id, collapse=","),
      if (is.character(roja)) roja else nrow(roja),
      if (is.character(roja)) "n/a" else paste(names(roja), collapse=","))
  }, error = function(e) sprintf("DIAG ERROR: %s", conditionMessage(e)))
  res
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
