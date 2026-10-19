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
    u <- tryCatch(get_user(user), error = function(e) NULL)
    if (is.null(u) || nrow(u) == 0) {
      return(list(result = FALSE))
    }
    ok <- tryCatch(bcrypt::checkpw(password, u$password_hash), error = function(e) FALSE)
    if (!ok) return(list(result = FALSE))
    tryCatch(touch_login(user), error = function(e) NULL)
    list(result = TRUE,
         user_info = list(user = user, role = u$role, trader_id = u$trader_id))
  }
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
                 tags_top = tags$h3("NAX Trader Dashboard"),
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
