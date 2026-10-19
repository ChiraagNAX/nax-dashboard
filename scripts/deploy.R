# Deploy to shinyapps.io under k95y82-chiraag-nax account.
# Prerequisites:
#   1. install.packages('rsconnect')
#   2. rsconnect::setAccountInfo(name='k95y82-chiraag-nax', token='...', secret='...')
#      (get token/secret from https://www.shinyapps.io/admin/#/tokens)
#   3. Add env vars via the shinyapps.io dashboard, or pass via setEnvVars below.

suppressPackageStartupMessages(library(rsconnect))

# These env vars must exist locally when deploying so rsconnect pushes them.
needed <- c("NAX_PG_HOST", "NAX_PG_PORT", "NAX_PG_DB", "NAX_PG_USER", "NAX_PG_PASSWORD")
missing <- needed[Sys.getenv(needed) == ""]
if (length(missing) > 0) stop("Missing env vars: ", paste(missing, collapse = ", "))

files <- c("app.R", ".Renviron",
           list.files("R", full.names = TRUE))
www <- list.files("www", full.names = TRUE, recursive = TRUE)
if (length(www) > 0) files <- c(files, www)

rsconnect::deployApp(
  appDir      = ".",
  appName     = "nax-dashboard",
  account     = "k95y82-chiraag-nax",
  appFiles    = files,
  forceUpdate = TRUE
)
