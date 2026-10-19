suppressPackageStartupMessages({ library(DBI); library(RPostgres); library(bcrypt) })
readRenviron(".Renviron")

users <- list(
  list(user_id="chiraag", password="NaxAdmin-2026-Qk7Zm9Fw2Rv8Lp3",  role="admin",  trader_id=NA),
  list(user_id="roja",    password="RojaProp-2026-Pm3Vk8Qw9Rt4Lx2",  role="trader", trader_id="roja")
)

connect <- function() dbConnect(RPostgres::Postgres(),
  host=Sys.getenv("NAX_PG_HOST"),
  port=as.integer(Sys.getenv("NAX_PG_PORT")),
  dbname=Sys.getenv("NAX_PG_DB"),
  user=Sys.getenv("NAX_PG_USER"),
  password=Sys.getenv("NAX_PG_PASSWORD"),
  sslmode="require",
  check_interrupts=FALSE)

for (u in users) {
  h <- bcrypt::hashpw(u$password)
  con <- connect()
  dbExecute(con, "
    insert into users (user_id, password_hash, role, trader_id)
    values ($1,$2,$3,$4)
    on conflict (user_id) do update set
      password_hash=excluded.password_hash,
      role=excluded.role,
      trader_id=excluded.trader_id
  ", params=list(u$user_id, h, u$role, u$trader_id))
  dbDisconnect(con)
  cat(sprintf("%s (%s) seeded\n", u$user_id, u$role))
}
