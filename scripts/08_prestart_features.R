# ============================================================================
# 08_prestart_features.R
# Clinical Trial Forecaster - How much signal is left before a trial starts?
# ============================================================================
# Purpose: The published model uses final (actual) enrollment, sites and
#          countries, which are known only once a trial is under way or
#          finished. This refits the same Elastic Net with only features
#          known at study start (phase, sponsor type, registration-to-start
#          lag) and compares both feature sets on the same splits as
#          06_random_vs_time_split.R.
# Output:  app/models/prestart_comparison.csv
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
full_recipe     <- readRDS(here("app", "data-raw", "modeling", "trial_recipe.rds"))
split_info      <- readRDS(here("app", "models", "temporal_split_info.rds"))
cohort <- trials_features |> filter(start_year <= split_info$test_end)

# Features known at study start: no enrollment, sites or countries
prestart_recipe <- recipe(
  log_duration ~ registration_to_start + phase_simple + sponsor_simple,
  data = cohort
) |>
  step_impute_median(all_numeric_predictors()) |>
  step_impute_mode(all_nominal_predictors()) |>
  step_normalize(all_numeric_predictors()) |>
  step_dummy(all_nominal_predictors(), one_hot = FALSE) |>
  step_zv(all_predictors())

rmse_vec <- function(t, e) sqrt(mean((t - e)^2))

# Same tuning as scripts 04-06: 10-fold CV, 5 x 5 penalty/mixture grid
fit_enet <- function(train, rec, seed = 2026) {
  set.seed(seed)
  folds <- vfold_cv(train, v = 10, strata = phase_simple)
  grid  <- expand.grid(penalty = 10^seq(-4, 0, length.out = 5),
                       mixture = seq(0, 1, length.out = 5))
  cv <- sapply(seq_len(nrow(grid)), function(i) {
    wf <- workflow() |> add_recipe(rec) |>
      add_model(linear_reg(penalty = grid$penalty[i], mixture = grid$mixture[i]) |>
                  set_engine("glmnet"))
    mean(sapply(folds$splits, function(s) {
      f <- fit(wf, analysis(s)); a <- assessment(s)
      rmse_vec(a$log_duration, predict(f, a)$.pred)
    }))
  })
  best <- grid[which.min(cv), ]
  workflow() |> add_recipe(rec) |>
    add_model(linear_reg(penalty = best$penalty, mixture = best$mixture) |> set_engine("glmnet")) |>
    fit(train)
}

score <- function(fit, test) {
  y <- test$log_duration; p <- predict(fit, test)$.pred
  c(rsq_trad = 1 - sum((y - p)^2) / sum((y - mean(y))^2),
    rsq_cor = cor(y, p)^2,
    median_abs_err_mo = median(abs(exp(y) - exp(p))))
}

run_split <- function(train, test, label, seed = 2026) {
  bind_rows(
    c(features = "all (published model)", split = label, as.list(score(fit_enet(train, full_recipe, seed), test))),
    c(features = "pre-start only", split = label, as.list(score(fit_enet(train, prestart_recipe, seed), test)))
  )
}

train_t <- cohort |> filter(start_year <= split_info$train_cutoff)
test_t  <- cohort |> filter(start_year >= split_info$test_start)
prop_test <- nrow(test_t) / nrow(cohort)

res <- bind_rows(
  run_split(train_t, test_t, "time-based"),
  bind_rows(lapply(2026:2030, function(seed) {
    set.seed(seed)
    s <- initial_split(cohort, prop = 1 - prop_test, strata = phase_simple)
    run_split(training(s), testing(s), "random", seed)
  }))
) |>
  mutate(across(c(rsq_trad, rsq_cor, median_abs_err_mo), as.numeric)) |>
  group_by(features, split) |>
  summarise(across(c(rsq_trad, rsq_cor, median_abs_err_mo), \(x) round(mean(x), 3)),
            n_runs = n(), .groups = "drop")

print(as.data.frame(res))
write.csv(res, here("app", "models", "prestart_comparison.csv"), row.names = FALSE)
cat("\nSaved: app/models/prestart_comparison.csv\n")
