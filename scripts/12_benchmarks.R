# ============================================================================
# 12_benchmarks.R
# Clinical Trial Forecaster - Simple benchmarks
# ============================================================================
# Purpose: Does the Elastic Net beat rules a planner could apply by hand?
#          Same cohort and splits as 06_random_vs_time_split.R (time-based:
#          train <= 2015, test 2016-2018; five random splits, seeds 2026-2030,
#          stratified by phase, same test-set size). Benchmarks, all on the
#          log-duration scale and fitted on the training trials only:
#            - constant: mean log duration of the training trials
#            - phase median: median log duration by phase
#            - phase x sponsor median: by phase and sponsor type, falling back
#              to the phase median for cells with fewer than 10 trials
#          Metrics as in script 06. The Elastic Net rows are read from
#          app/models/split_comparison.csv (script 06), not refitted.
# Output:  app/models/benchmark_comparison.csv
# Author:  Steven Ponce
# Date:    October 2026
# ============================================================================

library(here)
library(dplyr)
library(rsample)

trials_features <- readRDS(here("app", "data-raw", "modeling", "trials_features.rds"))
split_info      <- readRDS(here("app", "models", "temporal_split_info.rds"))
cohort <- trials_features |> filter(start_year <= split_info$test_end)

score <- function(y, p, label, split) {
  err <- exp(y) - exp(p)
  tibble(
    split = split, model = label, test_n = length(y),
    rsq_cor = if (sd(p) > 0) cor(y, p)^2 else NA_real_,
    rsq_trad = 1 - sum((y - p)^2) / sum((y - mean(y))^2),
    median_abs_err_mo = median(abs(err)),
    median_err_mo = median(err)
  )
}

benchmarks <- function(train, test, split) {
  y <- test$log_duration
  const <- rep(mean(train$log_duration), nrow(test))
  ph <- tapply(train$log_duration, train$phase_simple, median)
  p_phase <- unname(ph[as.character(test$phase_simple)])
  cell <- train |>
    group_by(phase_simple, sponsor_simple) |>
    summarise(n = n(), med = median(log_duration), .groups = "drop")
  key <- paste(test$phase_simple, test$sponsor_simple)
  cell_med <- setNames(cell$med, paste(cell$phase_simple, cell$sponsor_simple))
  cell_n <- setNames(cell$n, names(cell_med))
  p_cell <- ifelse(!is.na(cell_n[key]) & cell_n[key] >= 10, cell_med[key], p_phase)
  bind_rows(
    score(y, const, "Constant (training mean)", split),
    score(y, p_phase, "Phase median", split),
    score(y, unname(p_cell), "Phase x sponsor median", split)
  )
}

train_t <- cohort |> filter(start_year <= split_info$train_cutoff)
test_t  <- cohort |> filter(start_year >= split_info$test_start)
res <- list(benchmarks(train_t, test_t, "time-based"))

prop_test <- nrow(test_t) / nrow(cohort)
for (seed in 2026:2030) {
  set.seed(seed)
  s <- initial_split(cohort, prop = 1 - prop_test, strata = phase_simple)
  res[[length(res) + 1]] <- benchmarks(training(s), testing(s), paste0("random (seed ", seed, ")"))
}
res <- bind_rows(res)

random_mean <- res |>
  filter(grepl("^random", split)) |>
  group_by(model) |>
  summarise(across(c(rsq_cor, rsq_trad, median_abs_err_mo, median_err_mo), mean), .groups = "drop") |>
  mutate(split = "random (mean of 5)", test_n = NA_integer_)

enet <- read.csv(here("app", "models", "split_comparison.csv")) |>
  filter(split %in% c("time-based", "random (mean of 5)")) |>
  transmute(split, model = "Elastic Net (all features)", test_n, rsq_cor, rsq_trad,
            median_abs_err_mo, median_err_mo)

out <- bind_rows(res, random_mean, enet) |>
  filter(split %in% c("time-based", "random (mean of 5)")) |>
  mutate(across(where(is.double), ~ round(.x, 3))) |>
  arrange(split, desc(model == "Elastic Net (all features)"))

print(as.data.frame(out))
write.csv(out, here("app", "models", "benchmark_comparison.csv"), row.names = FALSE)
cat("\nSaved: app/models/benchmark_comparison.csv\n")
