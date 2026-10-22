# ============================================================================
# 13_regression_checks.R
# Clinical Trial Forecaster - Regression checks
# ============================================================================
# Purpose: Guard against the errors found in the October 2026 audit coming
#          back after a rebuild. Stops with an error if any check fails.
#            1. Month-only registry dates parse to the first of the month
#            2. One row per trial (no Phase 2 / Phase 3 duplicates)
#            3. Dates and durations agree and fall in the stated range
#            4. Split boundaries: train <= 2015, test 2016-2018, no overlap,
#               counts match the saved split
#            5. The model uses only the stated predictors, all available
#               for every trial
#            6. Registration-to-start lag is reproduced from the dates, and
#               the reduced-feature baseline does not use it (most trials
#               were registered after they started, so it is not a
#               pre-start input)
# Run:     Rscript scripts/13_regression_checks.R (also run by run_all.R)
# Author:  Steven Ponce
# Date:    October 2026
# ============================================================================

library(here)
library(dplyr)

for (e in parse(here("scripts", "02_download_and_clean.R"))) {
  if (is.call(e) && identical(e[[1]], as.name("<-")) &&
      identical(as.character(e[[2]]), "parse_registry_date")) eval(e)
}

n_fail <- 0
check <- function(ok, label) {
  cat(if (isTRUE(ok)) "PASS" else "FAIL", "-", label, "\n")
  if (!isTRUE(ok)) n_fail <<- n_fail + 1
}

# 1. Date parsing ------------------------------------------------------------
check(identical(parse_registry_date("2005-08"), as.Date("2005-08-01")),
      "month-only date '2005-08' parses to 2005-08-01 (not 2020-05-08)")
check(identical(parse_registry_date("2005-08-15"), as.Date("2005-08-15")),
      "full date parses unchanged")
check(is.na(suppressWarnings(parse_registry_date("Aug 2005"))),
      "unexpected format becomes NA, not a wrong date")

tf <- readRDS(here("app", "data-raw", "modeling", "trials_features.rds"))

# 2. Unique trials -----------------------------------------------------------
check(!anyDuplicated(tf$nct_id), paste0("one row per trial (", nrow(tf), " unique NCT IDs)"))

# 3. Dates and durations -----------------------------------------------------
reparsed_start <- parse_registry_date(tf$start_date)
check(all(reparsed_start == tf$start_date_parsed, na.rm = TRUE) && !anyNA(tf$start_date_parsed),
      "stored start dates match a fresh parse of the raw registry strings")
check(all(as.numeric(format(tf$start_date_parsed, "%Y")) == tf$start_year),
      "start_year matches the parsed start date")
dur <- as.numeric(tf$completion_date_parsed - tf$start_date_parsed) / 30.4375
check(max(abs(dur - tf$duration_months)) < 1, "duration_months matches completion - start")
check(all(tf$duration_months >= 1 & tf$duration_months <= 180), "durations within 1-180 months")
check(all(abs(tf$log_duration - log(tf$duration_months)) < 1e-6), "log_duration = log(duration_months)")

# 4. Split boundaries ---------------------------------------------------------
si <- readRDS(here("app", "models", "temporal_split_info.rds"))
train <- tf[tf$start_year <= si$train_cutoff, ]
test  <- tf[tf$start_year >= si$test_start & tf$start_year <= si$test_end, ]
check(si$train_cutoff == 2015 && si$test_start == 2016 && si$test_end == 2018,
      "split years: train <= 2015, test 2016-2018")
check(nrow(train) == si$train_n && nrow(test) == si$test_n,
      paste0("split sizes match the saved split (", nrow(train), " / ", nrow(test), ")"))
check(length(intersect(train$nct_id, test$nct_id)) == 0, "no trial in both train and test")

# 5. Model predictors ----------------------------------------------------------
rec <- readRDS(here("app", "data-raw", "modeling", "trial_recipe.rds"))
preds <- rec$var_info$variable[rec$var_info$role == "predictor"]
expected <- c("enrollment_count", "n_sites", "n_countries", "registration_to_start",
              "phase_simple", "sponsor_simple")
check(setequal(preds, expected), paste0("model predictors are exactly: ", paste(expected, collapse = ", ")))
check(all(preds %in% names(tf)), "every predictor exists in the modeling data")
check(!anyNA(tf[, c("phase_simple", "sponsor_simple")]), "phase and sponsor known for every trial")

# 6. Registration lag and the reduced-feature baseline ----------------------------
# The lag is start date minus first registry submission. It is negative for
# trials registered after they started, so for them it was not known at
# study start: it is a hindsight input like final enrollment, sites and
# countries, and the baseline in scripts/08 and 11 uses phase and sponsor only.
reg <- as.numeric(tf$start_date_parsed - as.Date(tf$first_submit_date))
check(max(abs(reg - tf$registration_to_start), na.rm = TRUE) < 1,
      "registration_to_start = start date - first registry submission")
modeled <- tf[tf$start_year <= si$test_end, ]
cat("INFO - trials registered after their start date (lag unknown at start):",
    sum(modeled$registration_to_start < 0, na.rm = TRUE), "of", nrow(modeled), "\n")
s08 <- paste(readLines(here("scripts", "08_prestart_features.R")), collapse = "\n")
check(grepl("baseline_recipe <- reduced_recipe(log_duration ~ phase_simple + sponsor_simple)", s08, fixed = TRUE),
      "scripts/08 baseline uses phase and sponsor only")
model_lines <- grep("(survreg|lm)\\(", readLines(here("scripts", "11_survival_model.R")), value = TRUE)
check(length(model_lines) >= 2 && !any(grepl("lag|registration", model_lines)),
      "scripts/11 survival models do not use registration lag")

cat("\n", if (n_fail == 0) "All checks passed." else paste(n_fail, "check(s) FAILED."), "\n")
if (n_fail > 0) quit(status = 1)
