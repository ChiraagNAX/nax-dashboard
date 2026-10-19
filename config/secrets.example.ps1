# Copy to secrets.local.ps1 and fill in. secrets.local.ps1 is gitignored.
# Supabase connection string (session pooler, port 6543 for serverless, 5432 for direct):
$env:NAX_PG_HOST     = 'aws-0-ap-southeast-2.pooler.supabase.com'
$env:NAX_PG_PORT     = '6543'
$env:NAX_PG_DB       = 'postgres'
$env:NAX_PG_USER     = 'postgres.your-project-ref'
$env:NAX_PG_PASSWORD = 'REPLACE_WITH_SUPABASE_PASSWORD'
