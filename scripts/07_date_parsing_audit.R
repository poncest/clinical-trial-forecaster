# ============================================================================
# 07_date_parsing_audit.R
# Clinical Trial Forecaster - Audit of the October 2026 date-parsing fix
# ============================================================================
# Purpose: Reproduce the counts quoted in the correction notes (README,
#          Methods tab, About box, blog). Compares the original parser
#          (lubridate::ymd) with parse_registry_date() on the cached January
#          2026 extract, applying the same cohort filters as scripts 02-03.
# Output:  app/models/date_parsing_audit.csv
# Author:  Steven Ponce
# Date:    October 2026
# ============================================================================

library(here)
library(dplyr)
library(lubridate)

trials_raw <- readRDS(here("app", "data-raw", "cache", "oncology_trials_extracted.rds"))
trials_df <- trials_raw |> distinct(nct_id, .keep_all = TRUE)  # as in script 02

# Same parser as scripts/02_data_exploration.R
parse_registry_date <- function(x) {
  x <- as.character(x)
  out <- rep(as.Date(NA), length(x))
  full <- !is.na(x) & grepl("^\\d{4}-\\d{2}-\\d{2}$", x)
  month_only <- !is.na(x) & grepl("^\\d{4}-\\d{2}$", x)
  out[full] <- as.Date(x[full], format = "%Y-%m-%d")
  out[month_only] <- as.Date(paste0(x[month_only], "-01"), format = "%Y-%m-%d")
  out
}

# Cohort filters from scripts 02 and 03
cohort <- function(start, completion) {
  trials_df |>
    mutate(start = start, completion = completion,
           duration_months = interval(start, completion) / months(1)) |>
    filter(study_type == "INTERVENTIONAL", primary_purpose == "TREATMENT",
           enrollment_type == "ACTUAL", !is.na(enrollment_count),
           !is.na(duration_months),
           duration_months >= 1, duration_months <= 180,
           enrollment_count >= 5, enrollment_count <= 10000) |>
    select(nct_id, start, completion, duration_months)
}

old <- suppressWarnings(cohort(ymd(trials_df$start_date), ymd(trials_df$completion_date)))
new <- cohort(parse_registry_date(trials_df$start_date),
              parse_registry_date(trials_df$completion_date))

both <- inner_join(old, new, by = "nct_id", suffix = c("_old", "_new"))
month_only <- grepl("^\\d{4}-\\d{2}$", trials_df$start_date) |
  grepl("^\\d{4}-\\d{2}$", trials_df$completion_date)

audit <- tibble(
  extracted_records       = nrow(trials_raw),
  duplicates_removed      = nrow(trials_raw) - nrow(trials_df),
  extracted_trials        = nrow(trials_df),
  month_only_date_trials  = sum(month_only),
  cohort_old_parser       = nrow(old),
  cohort_new_parser       = nrow(new),
  misdated_in_both        = sum(both$start_old != both$start_new |
                                  both$completion_old != both$completion_new),
  only_in_new_cohort      = nrow(anti_join(new, old, by = "nct_id")),
  only_in_old_cohort      = nrow(anti_join(old, new, by = "nct_id"))
)
print(t(audit))
write.csv(audit, here("app", "models", "date_parsing_audit.csv"), row.names = FALSE)
cat("\nSaved: app/models/date_parsing_audit.csv\n")
