# NAX Trader Dashboard — custom auth (no shinymanager)
suppressPackageStartupMessages({
  library(shiny); library(DT); library(bcrypt); library(jsonlite)
})

source("R/calc.R")
source("R/db.R")
source("R/ui_trader.R")
source("R/ui_admin.R")

login_ui <- div(
  style = "max-width:380px; margin:80px auto; padding:24px; border:1px solid #ddd; border-radius:8px; font-family:-apple-system,BlinkMacSystemFont,'Segoe UI',sans-serif;",
  h3("NAX Trader Dashboard", style = "margin-top:0; color:#2c3e50;"),
  p("Please sign in", style = "color:#666;"),
  textInput("login_user", "Username"),
  passwordInput("login_pw", "Password"),
  actionButton("login_btn", "Sign in", class = "btn-primary", width = "100%"),
  uiOutput("login_err")
)

ui <- fluidPage(
  tags$head(tags$title("NAX Trader Dashboard"),
            tags$style(HTML("body{font-family:-apple-system,BlinkMacSystemFont,'Segoe UI',sans-serif;}.well{background:#fafafa}h3{color:#2c3e50}"))),
  uiOutput("root")
)

server <- function(input, output, session) {
  auth <- reactiveValues(user_id = NULL, role = NULL, trader_id = NULL, err = NULL)

  observeEvent(input$login_btn, {
    user <- isolate(input$login_user)
    pw   <- isolate(input$login_pw)
    if (!nzchar(user) || !nzchar(pw)) {
      auth$err <- "Enter username and password."
      return()
    }
    u <- tryCatch(get_user(user), error = function(e) NULL)
    if (is.null(u) || nrow(u) == 0) {
      auth$err <- "Username or password is incorrect."
      return()
    }
    ok <- tryCatch(bcrypt::checkpw(pw, u$password_hash), error = function(e) FALSE)
    if (!ok) {
      auth$err <- "Username or password is incorrect."
      return()
    }
    tryCatch(touch_login(user), error = function(e) NULL)
    auth$user_id   <- as.character(user)
    auth$role      <- as.character(u$role)
    auth$trader_id <- if (is.na(u$trader_id)) NULL else as.character(u$trader_id)
    auth$err       <- NULL
  })

  output$login_err <- renderUI({
    if (is.null(auth$err)) NULL
    else div(style = "color:#c0392b; margin-top:8px;", auth$err)
  })

  user_id_rv   <- reactive({ auth$user_id })
  trader_id_rv <- reactive({ auth$trader_id })

  trader_server("trader", trader_id_rv)
  admin_server("admin",   user_id_rv)

  output$root <- renderUI({
    if (is.null(auth$user_id)) return(login_ui)
    if (identical(auth$role, "admin")) admin_ui("admin")
    else {
      req(auth$trader_id)
      trader_ui("trader")
    }
  })
}

shinyApp(ui, server)
