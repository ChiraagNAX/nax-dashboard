suppressPackageStartupMessages({ library(DBI); library(RPostgres) })
readRenviron(".Renviron")
cat("Host:", Sys.getenv("NAX_PG_HOST"), "Port:", Sys.getenv("NAX_PG_PORT"),
    "User:", Sys.getenv("NAX_PG_USER"), "\n")

con <- dbConnect(RPostgres::Postgres(),
  host=Sys.getenv("NAX_PG_HOST"),
  port=as.integer(Sys.getenv("NAX_PG_PORT")),
  dbname=Sys.getenv("NAX_PG_DB"),
  user=Sys.getenv("NAX_PG_USER"),
  password=Sys.getenv("NAX_PG_PASSWORD"),
  sslmode="require",
  check_interrupts=FALSE,
  bigint="integer")

cat("Connected.\n")
tryCatch({
  r <- dbGetQuery(con, "select 1 as one")
  print(r)
}, error=function(e) cat("select 1 failed:", conditionMessage(e), "\n"))

tryCatch({
  r <- dbGetQuery(con, "select current_user, current_database(), version()")
  print(r)
}, error=function(e) cat("version query failed:", conditionMessage(e), "\n"))

dbDisconnect(con)
