# Reports row-level-security status and whether Supabase's anon role can read each table.
suppressPackageStartupMessages({ library(DBI); library(RPostgres) })
setwd("C:/Users/nax-trader 2/Documents/nax-dashboard"); readRenviron(".Renviron")
con <- dbConnect(RPostgres::Postgres(),
  host=Sys.getenv("NAX_PG_HOST"), port=as.integer(Sys.getenv("NAX_PG_PORT")),
  dbname=Sys.getenv("NAX_PG_DB"), user=Sys.getenv("NAX_PG_USER"),
  password=Sys.getenv("NAX_PG_PASSWORD"), sslmode="require", check_interrupts=FALSE)
r <- dbGetQuery(con, "
  select t.tablename,
         t.rowsecurity as rls_on,
         has_table_privilege('anon', format('public.%I', t.tablename), 'select') as anon_can_select
  from pg_tables t where t.schemaname = 'public' order by t.tablename")
print(r)
dbDisconnect(con)
