# ============================================================================
# 10_download_all_statuses.R
# Clinical Trial Forecaster - Full pull for the censoring-aware model
# ============================================================================
# Purpose: The published cohort keeps only COMPLETED trials, and the January
#          2026 pull stopped at 2,000 trials per phase. A survival model needs
#          trials that are still running (censored) as well as finished ones.
#          This downloads EVERY Phase 2 and Phase 3 cancer trial that started
#          2000-2018, of any status, with no page cap.
# Run:     once, on a machine with internet access (~5-10 minutes):
#            Rscript scripts/10_download_all_statuses.R
#          then commit the output file below.
# Output:  app/data-raw/cache/oncology_trials_all_status.rds
#          (extracted fields only, plus the pull date; raw JSON is not kept)
# Author:  Steven Ponce
# Date:    October 2026
# ============================================================================

library(here)
library(httr2)
library(dplyr)
library(purrr)

# Reuse the field extraction from 02_download_and_clean.R so both pulls are
# parsed identically: evaluate only its top-level function definitions,
# `%||%` and base_url, without running the download or cleaning steps.
for (e in parse(here("scripts", "02_download_and_clean.R"))) {
  if (is.call(e) && identical(e[[1]], as.name("<-"))) {
    lhs <- as.character(e[[2]])
    rhs <- e[[3]]
    if (lhs %in% c("base_url", "%||%") ||
        (is.call(rhs) && identical(rhs[[1]], as.name("function")))) eval(e)
  }
}
stopifnot(exists("extract_all_trials"), exists("base_url"))

pull_phase <- function(phase, start_from = "1990-01-01", start_to = "2018-12-31") {
  filter_string <- sprintf("AREA[Phase]%s AND AREA[StartDate]RANGE[%s,%s]", phase, start_from, start_to)
  studies <- list(); token <- NULL; page <- 0
  repeat {
    page <- page + 1
    req <- request(base_url) |>
      req_url_query(pageSize = 1000, `query.cond` = "cancer", `filter.advanced` = filter_string) |>
      req_timeout(180) |>
      req_retry(max_tries = 4)
    if (!is.null(token)) req <- req |> req_url_query(pageToken = token)
    res <- req |> req_perform() |> resp_body_json()
    studies <- c(studies, res$studies)
    cat(sprintf("  %s page %d: %d trials (total %d)\n", phase, page, length(res$studies), length(studies)))
    token <- res$nextPageToken
    if (is.null(token)) break
    Sys.sleep(0.5)
  }
  extract_all_trials(studies)
}

pull_date <- Sys.Date()
cat("Downloading Phase 2...\n"); p2 <- pull_phase("PHASE2")
cat("Downloading Phase 3...\n"); p3 <- pull_phase("PHASE3")

all_status <- bind_rows(p2, p3) |>
  distinct(nct_id, .keep_all = TRUE)   # Phase 2/3 trials appear in both pulls
attr(all_status, "pull_date") <- pull_date

cat("\nTrials:", nrow(all_status), "\nBy status:\n")
print(sort(table(all_status$overall_status), decreasing = TRUE))

out <- here("app", "data-raw", "cache", "oncology_trials_all_status.rds")
saveRDS(all_status, out)
cat("\nSaved:", out, sprintf("(%.1f MB)", file.size(out) / 1e6), "\nPull date:", format(pull_date), "\n")
