suppressPackageStartupMessages({ library(DBI); library(RPostgres) })
readRenviron(".Renviron")

connect <- function() {
  dbConnect(RPostgres::Postgres(),
    host=Sys.getenv("NAX_PG_HOST"),
    port=as.integer(Sys.getenv("NAX_PG_PORT")),
    dbname=Sys.getenv("NAX_PG_DB"),
    user=Sys.getenv("NAX_PG_USER"),
    password=Sys.getenv("NAX_PG_PASSWORD"),
    sslmode="require",
    check_interrupts=FALSE,
    bigint="integer")
}

con <- connect()
cat("Tables:\n")
tbls <- dbGetQuery(con, "select tablename from pg_tables where schemaname='public' order by tablename")
print(tbls)
dbDisconnect(con)

con <- connect()
cat("\nLevels:\n")
tryCatch(print(dbGetQuery(con, "select level_code, display_name, capital_usd from levels")),
         error=function(e) cat("levels query failed:", conditionMessage(e), "\n"))
dbDisconnect(con)

con <- connect()
cat("\nTraders:\n")
tryCatch(print(dbGetQuery(con, "select trader_id, display_name, level_code from traders")),
         error=function(e) cat("traders query failed:", conditionMessage(e), "\n"))
dbDisconnect(con)
