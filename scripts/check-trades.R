suppressPackageStartupMessages({ library(DBI); library(RPostgres) })
setwd("C:/Users/nax-trader 2/Documents/nax-dashboard"); readRenviron(".Renviron")
con <- dbConnect(RPostgres::Postgres(),
  host=Sys.getenv("NAX_PG_HOST"), port=as.integer(Sys.getenv("NAX_PG_PORT")),
  dbname=Sys.getenv("NAX_PG_DB"), user=Sys.getenv("NAX_PG_USER"),
  password=Sys.getenv("NAX_PG_PASSWORD"), sslmode="require", check_interrupts=FALSE)
print(dbGetQuery(con, "select count(*) n, min(closed_at) first_close, max(closed_at) last_close,
                       round(sum(pnl_usd)::numeric,2) sum_pnl,
                       round(sum(open_fee_usd+close_fee_usd)::numeric,2) fees,
                       sum((pnl_usd>0)::int) wins, sum((pnl_usd<0)::int) losses from trades"))
print(dbGetQuery(con, "select symbol, count(*) n, round(sum(pnl_usd)::numeric,2) pnl from trades group by 1 order by 3 desc"))
print(dbGetQuery(con, "select date_trunc('day', closed_at) d, count(*) n, round(sum(pnl_usd)::numeric,2) pnl from trades group by 1 order by 1"))
print(dbGetQuery(con, "select count(*) filter (where closed_at < '2026-10-14') w1_trades,
                       round(coalesce(sum(pnl_usd) filter (where closed_at < '2026-10-14'),0)::numeric,2) w1_pnl,
                       round(coalesce(sum(pnl_usd) filter (where closed_at >= '2026-10-14'),0)::numeric,2) w2_pnl from trades"))
dbDisconnect(con)
