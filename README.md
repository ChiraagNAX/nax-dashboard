# NAX Trader Dashboard

Trader-facing dashboard for the NAX Capital Proprietary Trader Programme.

## Architecture

- **Snapshot writer** (`scripts/snapshot-writer.ps1`) — runs on the same Windows box as `nax-monitor`, polls Bybit per trader every 60s, writes snapshots to Postgres. Reuses the bot's `Get-AccountSnapshot` logic (same Bybit client, same exposure/leverage math), but computes **trailing HWM** and **transfer-adjusted equity** (bot does neither).
- **Postgres** (Supabase) — single source of truth for the dashboard. See `sql/schema.sql`.
- **Shiny app** (`app.R`, `R/`) — reads Postgres only. Never calls Bybit. Deployed to `k95y82-chiraag-nax.shinyapps.io/nax-dashboard`.
- **Shared calc module** (`R/calc.R`) — HWM, drawdown, exposure, payout. Unit-tested against the four worked examples in the spec.

## Secrets

Never committed. The writer reads `config/secrets.local.ps1` (gitignored); the Shiny app reads `.Renviron` locally and shinyapps.io env vars in prod. See `config/secrets.example.ps1` and `config/.Renviron.example`.

## Status

MVP in progress. First target: Roja's window closes 2026-10-14 00:00 UTC.


