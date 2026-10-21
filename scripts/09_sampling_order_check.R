# ============================================================================
# 09_sampling_order_check.R
# Clinical Trial Forecaster - Does the 2,000-per-phase cap cut by date?
# ============================================================================
# Purpose: The January 2026 pull kept the first 2,000 trials returned by each
#          phase query (Phase 2, Phase 3). If the API returned trials in date
#          order, the cap would bias the cohort by start year. This checks the
#          return order inside each pull. It cannot check the full registry
#          population, which would need a fresh count query.
# Output:  app/models/sampling_order_check.csv
# Author:  Steven Ponce
# Date:    October 2026
# ============================================================================

library(here)
library(dplyr)

d <- readRDS(here("app", "data-raw", "cache", "oncology_trials_extracted.rds")) |>
  mutate(position = row_number(),
         pull = if_else(position <= 2000, "Phase 2 query", "Phase 3 query"),
         start_year = as.numeric(substr(start_date, 1, 4)),
         nct_number = as.numeric(sub("NCT", "", nct_id)))

check <- d |>
  group_by(pull) |>
  mutate(quarter_of_pull = ntile(position, 4)) |>
  summarise(
    n = n(),
    cor_position_start_year = round(cor(position, start_year, use = "complete.obs"), 3),
    cor_position_nct_number = round(cor(position, nct_number, use = "complete.obs"), 3),
    median_start_year_q1 = median(start_year[quarter_of_pull == 1], na.rm = TRUE),
    median_start_year_q4 = median(start_year[quarter_of_pull == 4], na.rm = TRUE),
    first_nct = min(nct_id), last_nct = max(nct_id),
    .groups = "drop"
  )
print(as.data.frame(check))
write.csv(check, here("app", "models", "sampling_order_check.csv"), row.names = FALSE)
cat("\nSaved: app/models/sampling_order_check.csv\n")
