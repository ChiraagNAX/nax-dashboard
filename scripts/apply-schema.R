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

sql <- paste(readLines("sql/schema.sql", warn=FALSE), collapse="\n")
# strip -- line comments first (keeping newlines)
sql <- gsub("--[^\n]*", "", sql, perl=TRUE)
stmts <- strsplit(sql, ";\\s*\\n")[[1]]
stmts <- trimws(stmts)
stmts <- stmts[nchar(stmts) > 0]

ok <- 0; fail <- 0
for (s in stmts) {
  con <- tryCatch(connect(), error=function(e) NULL)
  if (is.null(con)) { fail <- fail+1; next }
  tryCatch({
    dbExecute(con, s)
    ok <- ok + 1
  }, error = function(e) {
    cat("FAIL:", conditionMessage(e), "\n   stmt head:", substr(s, 1, 80), "...\n")
    fail <<- fail + 1
  })
  try(dbDisconnect(con), silent=TRUE)
}
cat(sprintf("\nDone. ok=%d fail=%d\n", ok, fail))
