# Install everything the Shiny app and tooling need. Run once.
pkgs <- c(
  "shiny", "shinymanager", "DT", "bcrypt", "jsonlite",
  "DBI", "RPostgres", "pool",
  "testthat", "rsconnect"
)
to_install <- pkgs[!pkgs %in% rownames(installed.packages())]
if (length(to_install) > 0) {
  install.packages(to_install, repos = "https://cloud.r-project.org")
}
cat("All packages installed.\n")
