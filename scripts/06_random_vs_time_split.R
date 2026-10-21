# ============================================================================
# 06_random_vs_time_split.R
# Clinical Trial Forecaster - Random vs Time-Based Validation Comparison
# ============================================================================
# Purpose: Quantify how much the validation strategy changes the result,
#          holding everything else fixed.
# Method:  Same cohort (trials started 2018 or earlier), same recipe, same
#          Elastic Net grid as 04_modeling.R (5 x 5 penalty/mixture, best by
#          10-fold CV RMSE within the training data). The only difference is
#          the split:
#            - time-based: train <= 2015, test 2016-2018 (as in 04)
#            - random: 5 random splits with the same test-set size,
#              stratified by phase (seeds 2026-2030)
#          Metrics on the log-duration scale:
#            - rsq_cor: squared correlation (what yardstick::rsq reports)
#            - rsq_trad: 1 - SSE/SST (can be negative: worse than the mean)
#          plus median absolute error and median error in months.
# Output:  app/models/split_comparison.csv
# Author:  Steven Ponce
# Date:    October 2026
# ============================================================================

library(here)
library(dplyr)
library(rsample)
library(parsnip)
library(workflows)
library(recipes)

trials_features <- readRDS(here("app", "data-raw", "modeling", "trials_features.rds"))
trial_recipe    <- readRDS(here("app", "data-raw", "modeling", "trial_recipe.rds"))
split_info      <- readRDS(here("app", "models", "temporal_split_info.rds"))

cohort <- trials_features |> filter(start_year <= split_info$test_end)

rmse_vec <- function(t, e) sqrt(mean((t - e)^2))

fit_enet <- function(train, seed = 2026) {
  set.seed(seed)
  folds <- vfold_cv(train, v = 10, strata = phase_simple)
  grid  <- expand.grid(penalty = 10^seq(-4, 0, length.out = 5),
                       mixture = seq(0, 1, length.out = 5))
  cv <- sapply(seq_len(nrow(grid)), function(i) {
    wf <- workflow() |> add_recipe(trial_recipe) |>
      add_model(linear_reg(penalty = grid$penalty[i], mixture = grid$mixture[i]) |>
                  set_engine("glmnet"))
    mean(sapply(folds$splits, function(s) {
      f <- fit(wf, analysis(s)); a <- assessment(s)
      rmse_vec(a$log_duration, predict(f, a)$.pred)
    }))
  })
  best <- grid[which.min(cv), ]
  workflow() |> add_recipe(trial_recipe) |>
    add_model(linear_reg(penalty = best$penalty, mixture = best$mixture) |>
                set_engine("glmnet")) |>
    fit(train)
}

evaluate <- function(fit, test, label) {
  y <- test$log_duration
  p <- predict(fit, test)$.pred
  err <- exp(y) - exp(p)  # months, actual - predicted
  tibble(
    split = label,
    train_n = NA_integer_, test_n = nrow(test),
    rsq_cor = cor(y, p)^2,
    rsq_trad = 1 - sum((y - p)^2) / sum((y - mean(y))^2),
    rmse_log = rmse_vec(y, p),
    median_abs_err_mo = median(abs(err)),
    median_err_mo = median(err),
    share_overpredicted = mean(err < 0)
  )
}

# Time-based split (as in 04_modeling.R)
train_t <- cohort |> filter(start_year <= split_info$train_cutoff)
test_t  <- cohort |> filter(start_year >= split_info$test_start)
res_time <- evaluate(fit_enet(train_t), test_t, "time-based") |>
  mutate(train_n = nrow(train_t))

# Random splits with the same test-set size
prop_test <- nrow(test_t) / nrow(cohort)
res_random <- bind_rows(lapply(2026:2030, function(seed) {
  set.seed(seed)
  s <- initial_split(cohort, prop = 1 - prop_test, strata = phase_simple)
  evaluate(fit_enet(training(s), seed), testing(s), paste0("random (seed ", seed, ")")) |>
    mutate(train_n = nrow(training(s)))
}))

results <- bind_rows(res_time, res_random) |>
  mutate(across(where(is.double), ~ round(.x, 3)))

summary_row <- res_random |>
  summarise(across(c(rsq_cor, rsq_trad, rmse_log, median_abs_err_mo, median_err_mo,
                     share_overpredicted), mean)) |>
  mutate(split = "random (mean of 5)", train_n = NA_integer_, test_n = NA_integer_) |>
  mutate(across(where(is.double), ~ round(.x, 3)))

results <- bind_rows(results, summary_row)
print(as.data.frame(results))

write.csv(results, here("app", "models", "split_comparison.csv"), row.names = FALSE)
cat("\nSaved: app/models/split_comparison.csv\n")
