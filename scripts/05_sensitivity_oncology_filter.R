# ============================================================================
# 05_sensitivity_oncology_filter.R
# Clinical Trial Forecaster - Cohort Definition Sensitivity Check
# ============================================================================
# Purpose: Test whether non-oncology trials picked up by the keyword-based
#          registry query change the modeling conclusions.
# Method:  Mirrors the Elastic Net path of 04_modeling.R (time-based split,
#          10-fold CV stratified by phase on training data, 5x5 penalty/
#          mixture grid, best by CV RMSE, refit on full training set, single
#          evaluation on the 2016-2018 test set), run twice: on all trials and
#          on trials whose conditions field contains an oncology term.
#          CV is looped manually, so only rsample/parsnip/workflows are needed.
# Result:  see app/models/sensitivity_oncology_filter.csv (written below).
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

# ----------------------------------------------------------------------------
# Crude oncology flag: any cancer term in the conditions field.
# Errs in both directions: flags fibroids and arthritis as non-oncology
# correctly, but also flags e.g. "Recurrent GBM" or "ALK Gene Mutation".
# ----------------------------------------------------------------------------
onc_terms <- paste(
  "cancer|neoplas|tumou?r|carcinom|lymphom|leuka?emi|myelom|sarcom|melanom|glioma|glioblastom",
  "blastoma|mesotheliom|metasta|malignan|oncolog|myelodysplas|seminoma|myeloprolif",
  "macroglobulin|waldenstr|mycosis fungoides|lymphoblastic|myeloid|plasmacytoma|amyloidosis",
  "\\bcll\\b|\\baml\\b|\\ball\\b|\\bnsclc\\b|\\bsclc\\b|\\bmds\\b|hodgkin|adenoma|polycythemia|thrombocythemia",
  sep = "|"
)
trials_features$is_onc <- grepl(onc_terms, tolower(trials_features$conditions))

rmse_vec <- function(truth, estimate) sqrt(mean((truth - estimate)^2))
rsq_vec  <- function(truth, estimate) cor(truth, estimate)^2  # yardstick::rsq definition

run_enet <- function(data, label) {
  train <- data |> filter(start_year <= split_info$train_cutoff)
  test  <- data |> filter(start_year >= split_info$test_start, start_year <= split_info$test_end)

  set.seed(2026)
  folds <- vfold_cv(train, v = 10, strata = phase_simple)
  grid  <- expand.grid(penalty = 10^seq(-4, 0, length.out = 5),
                       mixture = seq(0, 1, length.out = 5))

  cv_rmse <- sapply(seq_len(nrow(grid)), function(i) {
    wf <- workflow() |>
      add_recipe(trial_recipe) |>
      add_model(linear_reg(penalty = grid$penalty[i], mixture = grid$mixture[i]) |>
                  set_engine("glmnet"))
    mean(sapply(folds$splits, function(s) {
      fit_i <- fit(wf, analysis(s))
      holdout <- assessment(s)
      rmse_vec(holdout$log_duration, predict(fit_i, holdout)$.pred)
    }))
  })

  best <- grid[which.min(cv_rmse), ]
  final_fit <- workflow() |>
    add_recipe(trial_recipe) |>
    add_model(linear_reg(penalty = best$penalty, mixture = best$mixture) |> set_engine("glmnet")) |>
    fit(train)

  pred <- predict(final_fit, test)$.pred
  err_months <- exp(test$log_duration) - exp(pred)  # actual - predicted

  coefs <- as.matrix(glmnet::coef.glmnet(extract_fit_engine(final_fit), s = best$penalty))[-1, 1]

  tibble(
    run                 = label,
    train_n             = nrow(train),
    test_n              = nrow(test),
    cv_rmse             = round(min(cv_rmse), 3),
    test_r2             = round(rsq_vec(test$log_duration, pred), 3),
    test_r2_trad        = round(1 - sum((test$log_duration - pred)^2) /
                                  sum((test$log_duration - mean(test$log_duration))^2), 3),
    test_rmse           = round(rmse_vec(test$log_duration, pred), 3),
    median_abs_err_mo   = round(median(abs(err_months)), 1),
    median_err_mo       = round(median(err_months), 1),
    share_overpredicted = round(mean(err_months < 0), 2),
    top3_predictors     = paste(names(sort(abs(coefs), decreasing = TRUE))[1:3], collapse = ", ")
  )
}

results <- bind_rows(
  run_enet(trials_features, "All trials (published model)"),
  run_enet(trials_features |> filter(is_onc), "Oncology terms only")
)

cat("\nTrials without an oncology term, by era:\n")
print(table(
  era = cut(trials_features$start_year,
            c(-Inf, split_info$train_cutoff, split_info$test_end, Inf),
            labels = c("train", "test", "excluded")),
  oncology_term = trials_features$is_onc
))

cat("\nSensitivity results:\n")
print(as.data.frame(t(results)))

# Saved for the app (Methods > Known Limitations) so the numbers are never retyped
modeled <- trials_features |> filter(start_year <= split_info$test_end)
results <- results |>
  mutate(n_modeled = nrow(modeled), n_no_oncology_term = sum(!modeled$is_onc))
write.csv(results, here("app", "models", "sensitivity_oncology_filter.csv"), row.names = FALSE)
cat("\nSaved: app/models/sensitivity_oncology_filter.csv\n")
