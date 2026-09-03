#!/usr/bin/env Rscript

# Compute shinyapps.io active-hours usage for the current billing window and
# write a shields.io "endpoint" badge JSON. Intended to run in CI (see
# .github/workflows/usage.yml) using the same SHINYAPPS_* secrets as deploy.
#
# Free tier = 25 active hours / month. The badge shows "X.X / 25 hrs" and is
# colored green/yellow/orange/red as usage approaches the cap.

suppressWarnings(suppressMessages({
  ok <- requireNamespace("rsconnect", quietly = TRUE) &&
    requireNamespace("jsonlite", quietly = TRUE)
}))
if (!ok) {
  stop("rsconnect and jsonlite are required", call. = FALSE)
}

free_tier_hours <- 25

account <- Sys.getenv("SHINYAPPS_NAME")
token <- Sys.getenv("SHINYAPPS_TOKEN")
secret <- Sys.getenv("SHINYAPPS_SECRET")
if (!nzchar(account) || !nzchar(token) || !nzchar(secret)) {
  stop("SHINYAPPS_NAME / SHINYAPPS_TOKEN / SHINYAPPS_SECRET must be set",
       call. = FALSE)
}

rsconnect::setAccountInfo(name = account, token = token, secret = secret)

# accountUsage returns a data frame with a "timestamp" column and an "hours"
# column: one row per summarization interval. Summing "hours" over the window
# gives total active hours consumed. We use a 30-day window as a proxy for the
# monthly billing cycle (shinyapps.io retains ~90 days of metrics).
usage <- rsconnect::accountUsage(
  account = account,
  server = "shinyapps.io",
  usageType = "hours",
  from = "30d",
  interval = "1d"
)

hours_used <- if (is.data.frame(usage) && "hours" %in% names(usage)) {
  sum(as.numeric(usage$hours), na.rm = TRUE)
} else {
  0
}
hours_used <- round(hours_used, 1)

pct <- hours_used / free_tier_hours
color <- if (pct >= 1) {
  "red"
} else if (pct >= 0.8) {
  "orange"
} else if (pct >= 0.5) {
  "yellow"
} else {
  "brightgreen"
}

badge <- list(
  schemaVersion = 1,
  label = "shinyapps.io usage",
  message = sprintf("%s / %d hrs (30d)", format(hours_used, nsmall = 1),
                    free_tier_hours),
  color = color
)

dir.create("badge", showWarnings = FALSE, recursive = TRUE)
jsonlite::write_json(
  badge, "badge/usage.json",
  auto_unbox = TRUE, pretty = TRUE
)

cat(sprintf("Active hours (last 30d): %s / %d (%s)\n",
            format(hours_used, nsmall = 1), free_tier_hours, color))

# Append a dated row to the usage-history CSV so the trend can be charted
# over time. The workflow reads this file from the badges branch before the
# script runs (see usage.yml) and republishes it afterward.
history_path <- "badge/usage-history.csv"
today <- format(Sys.Date(), "%Y-%m-%d")
new_row <- data.frame(
  date = today,
  hours_used = hours_used,
  free_tier_hours = free_tier_hours,
  pct = round(pct * 100, 1),
  stringsAsFactors = FALSE
)
if (file.exists(history_path)) {
  history <- utils::read.csv(history_path, stringsAsFactors = FALSE)
  # Replace any existing row for today so re-runs don't duplicate.
  history <- history[history$date != today, , drop = FALSE]
  history <- rbind(history, new_row)
} else {
  history <- new_row
}
history <- history[order(history$date), , drop = FALSE]
utils::write.csv(history, history_path, row.names = FALSE)

# Expose values to the GitHub Actions workflow (for the over-cap alert).
gh_output <- Sys.getenv("GITHUB_OUTPUT")
if (nzchar(gh_output)) {
  cat(
    sprintf("hours_used=%s\n", format(hours_used, nsmall = 1)),
    sprintf("pct=%d\n", round(pct * 100)),
    sprintf("free_tier_hours=%d\n", free_tier_hours),
    file = gh_output, append = TRUE, sep = ""
  )
}

# Emit a GitHub Actions job-summary block when running in CI.
summary_path <- Sys.getenv("GITHUB_STEP_SUMMARY")
if (nzchar(summary_path)) {
  bar_len <- 20
  filled <- max(0, min(bar_len, round(pct * bar_len)))
  bar <- paste0(strrep("#", filled), strrep("-", bar_len - filled))
  lines <- c(
    "## shinyapps.io active-hours usage",
    "",
    sprintf("**%s / %d active hours** used in the last 30 days.",
            format(hours_used, nsmall = 1), free_tier_hours),
    "",
    sprintf("`[%s]` %d%%", bar, round(pct * 100)),
    ""
  )
  cat(paste(lines, collapse = "\n"), file = summary_path, append = TRUE)
}
