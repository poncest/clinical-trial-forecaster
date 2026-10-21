# ============================================================================
# run_all.R
# Clinical Trial Forecaster - rebuild every artifact the app reads
# ============================================================================
# Usage (from the project root):  Rscript scripts/run_all.R
# Uses the cached January 2026 ClinicalTrials.gov snapshot in
# app/data-raw/cache/; set options(ctf.force_download = TRUE) in
# 02_download_and_clean.R's session to re-query the API instead.
# Script 01 only tests the API connection and is not needed here.
# Model tuning (04) takes several minutes.
# ============================================================================

library(here)

scripts <- c(
  "02_download_and_clean.R",          # cohort: parse dates, dedupe, clean
  "03_feature_engineering.R",         # features + preprocessing recipe
  "04_modeling.R",                    # CV comparison, final model, test set
  "05_sensitivity_oncology_filter.R", # cohort-definition sensitivity check
  "06_random_vs_time_split.R",        # random vs time-based (+ truncation-matched)
  "07_date_parsing_audit.R",          # counts behind the correction note
  "08_prestart_features.R",           # signal left with pre-start features only
  "09_sampling_order_check.R"         # does the API cap cut by date?
)

for (s in scripts) {
  message("\n=== ", s, " (", format(Sys.time(), "%H:%M:%S"), ")")
  # Each script runs in its own R process, exactly as when run by hand
  # (sourcing into one session would serialize that session into saved recipes)
  status <- system2(file.path(R.home("bin"), "Rscript"), shQuote(here("scripts", s)))
  if (status != 0) stop(s, " failed (exit status ", status, ")")
}
message("\nAll artifacts rebuilt in app/models/ and app/data-raw/.")
