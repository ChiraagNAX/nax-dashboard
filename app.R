# NAX Trader Dashboard — custom auth + bslib theme
suppressPackageStartupMessages({
  library(shiny); library(bslib); library(DT); library(bcrypt); library(jsonlite)
})

source("R/calc.R")
source("R/db.R")
source("R/theme.R")
source("R/ui_trader.R")
source("R/ui_admin.R")

login_ui <- div(class = "nax-login",
  div(class = "brand",
    div(class = "logo", "N"),
    h3("NAX Trader Dashboard"),
    div(class = "sub", "Proprietary Trader Programme")
  ),
  textInput("login_user", "Username", width = "100%"),
  passwordInput("login_pw", "Password", width = "100%"),
  actionButton("login_btn", "Sign in", class = "btn-primary", width = "100%"),
  uiOutput("login_err"),
  div(style = "text-align:center; margin-top:16px; font-size:11px; color:#94A3B8;",
      "Secured by shinymanager-compatible bcrypt · Session timeout 1 hour")
)

ui <- page_fluid(
  theme = nax_theme,
  tags$head(
    tags$title("NAX Trader Dashboard"),
    tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
    tags$link(rel = "stylesheet",
              href = "https://fonts.googleapis.com/css2?family=JetBrains+Mono:wght@400;500;600&display=swap"),
    tags$style(HTML(nax_css))
  ),
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
    else div(class = "err", auth$err)
  })

  user_id_rv   <- reactive({ auth$user_id })
  trader_id_rv <- reactive({ auth$trader_id })

  trader_server("trader", trader_id_rv)
  admin_server("admin",   user_id_rv)

  output$root <- renderUI({
    if (is.null(auth$user_id)) return(login_ui)
    if (identical(auth$role, "admin")) {
      div(style = "padding: 24px 32px;", admin_ui("admin"))
    } else {
      req(auth$trader_id)
      div(style = "padding: 20px 24px;",
          trader_ui("trader"),
          div(style = "text-align: right; margin-top: 20px;",
              actionLink("logout_btn", "Sign out",
                        style = "color:#64748B; font-size:12px;")))
    }
  })

  observeEvent(input$logout_btn, {
    auth$user_id <- NULL; auth$role <- NULL; auth$trader_id <- NULL
  })
}

shinyApp(ui, server)
