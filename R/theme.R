# NAX Dashboard theme + shared design tokens
suppressPackageStartupMessages(library(bslib))

nax_theme <- bs_theme(
  version    = 5,
  bg         = "#F8FAFC",
  fg         = "#0F172A",
  primary    = "#0EA5E9",   # sky
  secondary  = "#64748B",
  success    = "#10B981",
  info       = "#06B6D4",
  warning    = "#F59E0B",
  danger     = "#EF4444",
  base_font  = font_google("Inter"),
  heading_font = font_google("Inter"),
  font_scale = 0.95,
  "border-radius"       = "10px",
  "card-border-radius"  = "12px",
  "card-border-color"   = "#E2E8F0",
  "card-cap-bg"         = "transparent",
  "body-color"          = "#0F172A",
  "body-color-secondary" = "#475569"
)

nax_css <- "
  :root {
    --nax-navy: #0F172A;
    --nax-sky:  #0EA5E9;
    --nax-ok:   #10B981;
    --nax-warn: #F59E0B;
    --nax-bad:  #EF4444;
    --nax-mute: #64748B;
    --nax-surf: #FFFFFF;
    --nax-bg:   #F8FAFC;
    --nax-line: #E2E8F0;
  }
  body { background: var(--nax-bg); }
  .nax-mono { font-family: 'JetBrains Mono', ui-monospace, SFMono-Regular, monospace; font-variant-numeric: tabular-nums; }
  .nax-header {
    background: linear-gradient(135deg, #0F172A 0%, #1E293B 100%);
    color: white; padding: 20px 24px; border-radius: 14px; margin-bottom: 20px;
    box-shadow: 0 1px 2px rgba(15,23,42,0.08);
  }
  .nax-header h2 { margin: 0; font-weight: 600; letter-spacing: -0.02em; }
  .nax-header .badge-level {
    background: rgba(14,165,233,0.2); color: #7DD3FC;
    padding: 3px 10px; border-radius: 999px; font-size: 11px; font-weight: 600;
    letter-spacing: 0.04em; text-transform: uppercase; margin-left: 8px;
  }
  .nax-header .sub { color: #94A3B8; font-size: 13px; margin-top: 4px; }
  .nax-progress-track { height: 4px; background: rgba(255,255,255,0.1); border-radius: 2px; margin-top: 10px; }
  .nax-progress-fill { height: 4px; background: #0EA5E9; border-radius: 2px; }
  .nax-stale {
    background: #FEF3C7; color: #92400E; padding: 10px 14px; border-radius: 10px;
    border-left: 3px solid #F59E0B; margin-bottom: 16px; font-size: 13px;
  }
  .nax-kpi { padding: 16px 18px; background: white; border: 1px solid var(--nax-line);
             border-radius: 12px; height: 100%; }
  .nax-kpi .label { font-size: 11px; color: var(--nax-mute); text-transform: uppercase;
                    letter-spacing: 0.06em; font-weight: 600; }
  .nax-kpi .value { font-size: 28px; font-weight: 700; color: var(--nax-navy);
                    margin-top: 6px; letter-spacing: -0.02em; }
  .nax-kpi .delta { font-size: 13px; margin-top: 4px; font-weight: 500; }
  .nax-kpi.ok    .value { color: var(--nax-ok); }
  .nax-kpi.warn  .value { color: var(--nax-warn); }
  .nax-kpi.bad   .value { color: var(--nax-bad); }
  .delta-up   { color: var(--nax-ok); }
  .delta-down { color: var(--nax-bad); }
  .delta-flat { color: var(--nax-mute); }
  .nax-gauge { margin-bottom: 14px; }
  .nax-gauge .row-top { display: flex; justify-content: space-between; align-items: baseline;
                        font-size: 13px; margin-bottom: 6px; }
  .nax-gauge .row-top .lbl { color: var(--nax-mute); font-weight: 500; }
  .nax-gauge .row-top .val { font-weight: 600; }
  .nax-gauge .track { height: 6px; background: #F1F5F9; border-radius: 3px; overflow: hidden; }
  .nax-gauge .fill  { height: 6px; border-radius: 3px; transition: width 300ms ease; }
  .nax-gauge .fill.ok   { background: var(--nax-ok); }
  .nax-gauge .fill.warn { background: var(--nax-warn); }
  .nax-gauge .fill.bad  { background: var(--nax-bad); }
  .nax-badge { display: inline-block; padding: 3px 9px; border-radius: 999px;
               font-size: 11px; font-weight: 600; letter-spacing: 0.03em; }
  .nax-badge.pass    { background: #D1FAE5; color: #065F46; }
  .nax-badge.fail    { background: #FEE2E2; color: #991B1B; }
  .nax-badge.behind  { background: #FEF3C7; color: #92400E; }
  .nax-badge.pending { background: #E0F2FE; color: #075985; }
  .nax-tally { display: flex; gap: 20px; margin: 8px 0 16px; }
  .nax-tally .cell { flex: 1; text-align: center; padding: 10px; background: #F8FAFC; border-radius: 8px; }
  .nax-tally .cell .n { font-size: 22px; font-weight: 700; }
  .nax-tally .cell .l { font-size: 11px; color: var(--nax-mute); text-transform: uppercase;
                        letter-spacing: 0.05em; margin-top: 2px; }
  .nax-tally .cell.ok  .n { color: var(--nax-ok); }
  .nax-tally .cell.at  .n { color: var(--nax-warn); }
  .nax-tally .cell.over .n { color: var(--nax-bad); }
  .nax-payout-row { display: flex; justify-content: space-between; padding: 8px 0;
                    border-bottom: 1px solid #F1F5F9; font-size: 14px; }
  .nax-payout-row:last-child { border-bottom: none; }
  .nax-payout-row .k { color: var(--nax-mute); }
  .nax-payout-row .v { font-weight: 600; }
  .nax-payout-row.total { padding-top: 12px; margin-top: 4px; border-top: 2px solid var(--nax-navy);
                           border-bottom: none; font-size: 16px; }
  .nax-payout-row.total .v { color: var(--nax-navy); font-size: 18px; }
  .nax-footer { font-size: 11px; color: var(--nax-mute); text-align: center; padding: 20px 0; }
  .card { box-shadow: 0 1px 3px rgba(15,23,42,0.04); }
  .card-header { font-weight: 600; color: var(--nax-navy); font-size: 14px;
                  letter-spacing: -0.01em; padding: 14px 18px; border-bottom: 1px solid #F1F5F9; }
  .card-body { padding: 18px; }
  table.table-sm td { padding: 6px 4px; font-size: 13px; }
  table.table-sm td:first-child { color: var(--nax-mute); }
  table.table-sm td:last-child { font-weight: 500; text-align: right; }
  .nax-login {
    max-width: 400px; margin: 100px auto; padding: 32px;
    background: white; border-radius: 16px;
    box-shadow: 0 10px 30px rgba(15,23,42,0.08);
  }
  .nax-login .brand {
    text-align: center; margin-bottom: 24px;
  }
  .nax-login .brand .logo {
    width: 56px; height: 56px; border-radius: 14px;
    background: linear-gradient(135deg, #0F172A, #0EA5E9);
    color: white; display: inline-flex; align-items: center; justify-content: center;
    font-size: 24px; font-weight: 700; letter-spacing: -0.04em; margin-bottom: 12px;
  }
  .nax-login h3 { margin: 0; color: var(--nax-navy); font-weight: 600; }
  .nax-login .sub { color: var(--nax-mute); font-size: 13px; margin-top: 4px; }
  .nax-login .form-control { padding: 10px 12px; }
  .nax-login .btn-primary { background: var(--nax-navy); border-color: var(--nax-navy);
                             padding: 10px; font-weight: 600; }
  .nax-login .btn-primary:hover { background: #1E293B; border-color: #1E293B; }

  .nax-grid { display: grid; gap: 16px; margin-bottom: 16px; }
  .nax-grid > * { min-width: 0; }
  .nax-grid > .card { height: 100%; margin-bottom: 0; }
  .nax-grid.k4 { grid-template-columns: repeat(4, 1fr); }
  .nax-grid.c21 { grid-template-columns: 2fr 1fr; }
  .nax-grid.c3 { grid-template-columns: repeat(3, 1fr); }
  .nax-grid.c2 { grid-template-columns: repeat(2, 1fr); }
  @media (max-width: 991px) {
    .nax-grid.k4 { grid-template-columns: repeat(2, 1fr); }
    .nax-grid.c21, .nax-grid.c3, .nax-grid.c2 { grid-template-columns: 1fr; }
  }
  @media (max-width: 575px) {
    .container-fluid { padding-left: 12px; padding-right: 12px; }
    .nax-grid { gap: 10px; }
    .nax-kpi { padding: 12px 14px; }
    .nax-kpi .value { font-size: 21px; }
    .nax-header { padding: 16px; }
    .nax-header h2 { font-size: 22px; }
    .nax-payout-row { font-size: 13px; gap: 12px; }
    .nav-tabs { flex-wrap: nowrap; overflow-x: auto; white-space: nowrap; }
  }
  .recalculating { opacity: 1 !important; transition: none !important; }
  .nax-header .top { display: flex; justify-content: space-between; align-items: center; gap: 12px; }
  .nax-chip { font-size: 11px; font-weight: 600; letter-spacing: 0.04em; text-transform: uppercase;
              padding: 4px 10px 4px 20px; border-radius: 999px; position: relative; white-space: nowrap; }
  .nax-chip::before { content: ''; position: absolute; left: 8px; top: 50%; width: 7px; height: 7px;
                      margin-top: -3.5px; border-radius: 50%; background: currentColor; }
  .nax-chip.live { background: rgba(16,185,129,0.18); color: #6EE7B7; }
  .nax-chip.live::before { animation: nax-pulse 2s ease-in-out infinite; }
  .nax-chip.delayed { background: rgba(245,158,11,0.2); color: #FCD34D; }
  .nax-chip.stale { background: rgba(239,68,68,0.2); color: #FCA5A5; }
  @keyframes nax-pulse { 0%,100% { opacity: 1; } 50% { opacity: 0.3; } }
  @media (prefers-reduced-motion: reduce) { .nax-chip.live::before { animation: none; }
    .nax-gauge .fill { transition: none; } }
  .nax-section-head { display: flex; justify-content: space-between; align-items: center;
                      flex-wrap: wrap; gap: 8px; margin: 8px 0 8px; }
  .nax-section-head h5 { margin: 0; color: var(--nax-navy); font-weight: 600; }
  .nax-section-head .shiny-input-container { margin-bottom: 0; width: auto; }
  .nax-payout-row .v.pos { color: var(--nax-ok); }
  .nax-payout-row .v.neg { color: var(--nax-bad); }
  .nax-payout-row .k { padding-right: 12px; }
  .nax-kpi { transition: box-shadow 150ms ease; }
  .nax-kpi:hover { box-shadow: 0 2px 8px rgba(15,23,42,0.06); }
  table.table td, table.table th { font-variant-numeric: tabular-nums; }
  table.table-sm th { font-size: 11px; color: var(--nax-mute); text-transform: uppercase;
                      letter-spacing: 0.05em; font-weight: 600; }
  :focus-visible { outline: 2px solid var(--nax-sky); outline-offset: 2px; }
  .nax-login .brand .sub, .nax-footer a { color: var(--nax-mute); }
  .nax-footer a:hover { color: var(--nax-sky); }
  .nax-login .err { color: var(--nax-bad); font-size: 13px; margin-top: 10px; }
"

# ---- helpers for building UI widgets ----

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || (length(a) == 1 && is.na(a))) b else a

fmt_usd <- function(x, digits = 0, sign = FALSE) {
  vapply(x, function(v) {
    if (is.null(v) || is.na(v)) return("n/a")
    s <- if (v < 0) "-" else if (sign && v > 0) "+" else ""
    sprintf("%s$%s", s, format(round(abs(v), digits), big.mark = ",", nsmall = digits))
  }, "")
}
fmt_pct <- function(x, digits = 2, sign = FALSE) {
  vapply(x, function(v) {
    if (is.null(v) || is.na(v)) return("n/a")
    sprintf(paste0("%", if (sign) "+" else "", ".", digits, "f%%"), v)
  }, "")
}

status_of_pct <- function(pct_of_limit) {
  if (is.null(pct_of_limit) || is.na(pct_of_limit)) return("ok")
  if (pct_of_limit >= 100) "bad" else if (pct_of_limit >= 75) "warn" else "ok"
}

nax_kpi <- function(label, value, delta = NULL, status = "ok") {
  div(class = sprintf("nax-kpi %s", status),
    div(class = "label", label),
    div(class = "value nax-mono", value),
    if (!is.null(delta)) div(class = "delta", HTML(delta))
  )
}

nax_gauge <- function(label, value_text, pct_of_limit) {
  stat <- status_of_pct(pct_of_limit)
  pct  <- if (is.null(pct_of_limit) || is.na(pct_of_limit)) 0 else pct_of_limit
  div(class = "nax-gauge",
    div(class = "row-top",
      span(class = "lbl", label),
      span(class = "val nax-mono", value_text)
    ),
    div(class = "track",
      div(class = sprintf("fill %s", stat),
          style = sprintf("width:%.0f%%", min(100, max(0, pct))))
    )
  )
}

nax_tally <- function(crit, maj, soft, max_crit = 0, max_maj = 1, max_soft = 1) {
  cls <- function(n, m) if (n > m) "over" else if (n == m) "at" else "ok"
  div(class = "nax-tally",
    div(class = sprintf("cell %s", cls(crit, max_crit)),
      div(class = "n nax-mono", sprintf("%d/%d", crit, max_crit)),
      div(class = "l", "Critical")),
    div(class = sprintf("cell %s", cls(maj, max_maj)),
      div(class = "n nax-mono", sprintf("%d/%d", maj, max_maj)),
      div(class = "l", "Major")),
    div(class = sprintf("cell %s", cls(soft, max_soft)),
      div(class = "n nax-mono", sprintf("%d/%d", soft, max_soft)),
      div(class = "l", "Soft DD"))
  )
}

nax_row <- function(k, v, cls = "") {
  v <- as.character(v)
  sg <- if (grepl("^[+][$0-9]", v)) "pos" else if (grepl("^-[$0-9]", v)) "neg" else ""
  div(class = paste("nax-payout-row", cls),
      span(class = "k", k), span(class = paste("v nax-mono", sg), v))
}

nax_verdict_badge <- function(verdict) {
  cls <- switch(verdict,
    "PASS" = "pass", "FAIL" = "fail", "BEHIND" = "behind", "pending")
  span(class = sprintf("nax-badge %s", cls), verdict)
}
