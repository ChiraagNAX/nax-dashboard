# Usage: Rscript scripts/set-password.R <user_id> [role] [trader_id]
# Generates a random password, stores its bcrypt hash and prints the password once.
# Passwords are never kept in the repo.
suppressPackageStartupMessages({ library(DBI); library(RPostgres); library(bcrypt) })
readRenviron(".Renviron")
a <- commandArgs(TRUE)
uid <- a[1]; stopifnot(!is.na(uid))
chars <- c(letters, LETTERS, 0:9)
pw <- paste(sample(chars, 20, replace = TRUE), collapse = "")
con <- dbConnect(RPostgres::Postgres(), host = Sys.getenv("NAX_PG_HOST"),
  port = as.integer(Sys.getenv("NAX_PG_PORT")), dbname = Sys.getenv("NAX_PG_DB"),
  user = Sys.getenv("NAX_PG_USER"), password = Sys.getenv("NAX_PG_PASSWORD"),
  sslmode = "require", check_interrupts = FALSE)
n <- dbExecute(con, "update users set password_hash = $1 where user_id = $2",
               params = list(bcrypt::hashpw(pw), uid))
if (n == 0 && length(a) >= 2) {
  dbExecute(con, "insert into users (user_id, password_hash, role, trader_id) values ($1,$2,$3,$4)",
            params = list(uid, bcrypt::hashpw(pw), a[2], if (length(a) >= 3) a[3] else NA))
  n <- 1
}
dbExecute(con, "insert into admin_audit (actor, action, target_table, target_id, note)
                values ('script','reset_password','users',$1,'password set via set-password.R')",
          params = list(uid))
dbDisconnect(con)
if (n == 0) stop("no such user: ", uid)
cat(uid, "password:", pw, "\n")
