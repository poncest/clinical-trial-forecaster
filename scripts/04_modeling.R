# ============================================================================
# 04_modeling.R
# Clinical Trial Forecaster - Model Training & Evaluation
# ============================================================================
# Purpose: Train, tune, compare, and select best forecasting model
# Stage 3: Time-based validation (train ≤2015, test 2016-2018; 2019+ excluded)
# Author:  Steven Ponce
# Date:    January 2026
# ============================================================================

# ----------------------------------------------------------------------------
# Setup
# ----------------------------------------------------------------------------
library(here)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(tidymodels)
library(ranger)
library(xgboost)
library(glmnet)
library(vip)
library(fs)

# Set seed for reproducibility
set.seed(2026)

# Create output directories (all outputs go to app/ folder)
dir_create(here("app", "models"))
dir_create(here("app", "docs"))

# Tidymodels settings
tidymodels_prefer()  # Resolve function conflicts in favor of tidymodels

cat("\n")
cat("============================================================\n")
cat("🤖 Model Training & Evaluation - STAGE 3\n")
cat("============================================================\n")
cat("Clinical Trial Duration Forecasting\n")
cat("Time-Based Validation: Train ≤2015, Test 2016-2018\n")
cat("============================================================\n\n")

# ----------------------------------------------------------------------------
# Step 1: Load Data & Recipe
# ----------------------------------------------------------------------------

cat("STEP 1: Load Data & Recipe\n")
cat("------------------------------------------------------------\n\n")

trials_features <- readRDS(here("app", "data-raw", "modeling", "trials_features.rds"))
trial_recipe <- readRDS(here("app", "data-raw", "modeling", "trial_recipe.rds"))

cat("📊 Loaded", nrow(trials_features), "trials with", ncol(trials_features), "features\n")
cat("📊 Target: log_duration (log-transformed trial duration in months)\n\n")

# Verify start_year exists
if (!"start_year" %in% names(trials_features)) {
  stop("ERROR: start_year column not found in trials_features. Run 03_feature_engineering.R first.")
}

# ----------------------------------------------------------------------------
# Step 2: Time-Based Train/Test Split (STAGE 3 CHANGE)
# ----------------------------------------------------------------------------

cat("STEP 2: Time-Based Train/Test Split\n")
cat("------------------------------------------------------------\n\n")

cat("📊 Year distribution in dataset:\n")
year_counts <- trials_features |>
  count(start_year) |>
  arrange(start_year)
print(year_counts)

cat("\n📊 Year range:", min(trials_features$start_year), "-", max(trials_features$start_year), "\n")

# Time-based split: Train on historical data, test on "future" data
# 
# IMPORTANT: Right-truncation bias consideration
# - Only COMPLETED trials are in the dataset
# - Trials starting 2019+ that completed by 2026 are disproportionately SHORT
# - Long trials (60+ months) starting 2019+ are still ongoing → not in data
#
# Solution: Use a test window where trials had enough time to complete naturally
# - Typical trial duration: ~60-70 months (~5-6 years)
# - Train: ≤2015 (trials with 11+ years to complete)
# - Test: 2016-2018 (trials with 8-10 years to complete by 2026)
# - Exclude: 2019+ (right-truncated - only short completers visible)

train_cutoff <- 2015
test_start <- 2016
test_end <- 2018

train_data <- trials_features |> filter(start_year <= train_cutoff)
test_data <- trials_features |> filter(start_year >= test_start & start_year <= test_end)

# Also save excluded trials count for documentation
excluded_recent <- trials_features |> filter(start_year > test_end) |> nrow()

cat("\n📊 TIME-BASED SPLIT (avoiding right-truncation bias):\n")
cat("   Training set (≤", train_cutoff, "):", nrow(train_data), "trials\n")
cat("   Test set (", test_start, "-", test_end, "):", nrow(test_data), "trials\n")
cat("   Excluded (>", test_end, "):", excluded_recent, "trials (right-truncated)\n")
cat("   Train/Test ratio:", round(nrow(train_data) / (nrow(train_data) + nrow(test_data)) * 100, 1), "/",
    round(nrow(test_data) / (nrow(train_data) + nrow(test_data)) * 100, 1), "%\n")

# Verify we have enough data in both sets
if (nrow(train_data) < 100) {
  warning("WARNING: Training set has < 100 trials. Consider adjusting cutoff.")
}
if (nrow(test_data) < 50) {
  warning("WARNING: Test set has < 50 trials. Consider adjusting cutoff.")
}

# Save temporal split info for documentation
temporal_split_info <- list(
  train_cutoff = train_cutoff,
  test_start = test_start,
  test_end = test_end,
  train_n = nrow(train_data),
  test_n = nrow(test_data),
  excluded_recent_n = excluded_recent,
  train_years = sort(unique(train_data$start_year)),
  test_years = sort(unique(test_data$start_year)),
  total_n = nrow(trials_features),
  note = "Trials after test_end excluded due to right-truncation bias"
)

# Phase distribution check (may differ between periods)
cat("\n📊 Phase distribution check:\n")
cat("   Training (≤", train_cutoff, "):\n")
print(prop.table(table(train_data$phase_simple)) |> round(3))
cat("   Test (", test_start, "-", test_end, "):\n")
print(prop.table(table(test_data$phase_simple)) |> round(3))

# Sponsor distribution check
cat("\n📊 Sponsor distribution check:\n")
cat("   Training (≤", train_cutoff, "):\n")
print(prop.table(table(train_data$sponsor_simple)) |> round(3))
cat("   Test (", test_start, "-", test_end, "):\n")
print(prop.table(table(test_data$sponsor_simple)) |> round(3))

# Duration distribution check (key - are durations trending?)
cat("\n📊 Duration statistics by period:\n")
cat("   Training median:", round(median(train_data$duration_months), 1), "months\n")
cat("   Test median:", round(median(test_data$duration_months), 1), "months\n")
cat("   Training mean:", round(mean(train_data$duration_months), 1), "months\n")
cat("   Test mean:", round(mean(test_data$duration_months), 1), "months\n")

# ----------------------------------------------------------------------------
# Step 3: Cross-Validation Folds (within training data only)
# ----------------------------------------------------------------------------

cat("\n")
cat("STEP 3: Cross-Validation Setup (within training data)\n")
cat("------------------------------------------------------------\n\n")

# 10-fold CV on training data only, stratified by phase
cv_folds <- vfold_cv(
  train_data, 
  v = 10, 
  strata = phase_simple
)

cat("📊 Created 10-fold cross-validation on TRAINING data only\n")
cat("📊 Each fold: ~", round(nrow(train_data) / 10), "trials\n")
cat("📊 Stratified by: phase_simple\n\n")

# ----------------------------------------------------------------------------
# Step 4: Define Model Specifications
# ----------------------------------------------------------------------------

cat("STEP 4: Define Model Specifications\n")
cat("------------------------------------------------------------\n\n")

# --- Model 1: Linear Regression (Baseline) ---
lm_spec <- linear_reg() |>
  set_engine("lm") |>
  set_mode("regression")

cat("✅ Model 1: Linear Regression (baseline, no tuning)\n")

# --- Model 2: Elastic Net (Regularized Linear) ---
enet_spec <- linear_reg(
  penalty = tune(),
  mixture = tune()
) |>
  set_engine("glmnet") |>
  set_mode("regression")

cat("✅ Model 2: Elastic Net (tune: penalty, mixture)\n")

# --- Model 3: Random Forest ---
rf_spec <- rand_forest(
  mtry = tune(),
  trees = 500,
  min_n = tune()
) |>
  set_engine("ranger", importance = "impurity") |>
  set_mode("regression")

cat("✅ Model 3: Random Forest (tune: mtry, min_n; trees=500)\n")

# --- Model 4: XGBoost ---
xgb_spec <- boost_tree(
  trees = tune(),
  tree_depth = tune(),
  learn_rate = tune(),
  min_n = tune()
) |>
  set_engine("xgboost") |>
  set_mode("regression")

cat("✅ Model 4: XGBoost (tune: trees, depth, learn_rate, min_n)\n\n")

# ----------------------------------------------------------------------------
# Step 5: Create Workflows
# ----------------------------------------------------------------------------

cat("STEP 5: Create Workflows\n")
cat("------------------------------------------------------------\n\n")

# Workflow = Recipe + Model
lm_workflow <- workflow() |>
  add_recipe(trial_recipe) |>
  add_model(lm_spec)

enet_workflow <- workflow() |>
  add_recipe(trial_recipe) |>
  add_model(enet_spec)

rf_workflow <- workflow() |>
  add_recipe(trial_recipe) |>
  add_model(rf_spec)

xgb_workflow <- workflow() |>
  add_recipe(trial_recipe) |>
  add_model(xgb_spec)

cat("✅ Created 4 workflows (recipe + model)\n\n")

# ----------------------------------------------------------------------------
# Step 6: Define Tuning Grids
# ----------------------------------------------------------------------------

cat("STEP 6: Define Tuning Grids\n")
cat("------------------------------------------------------------\n\n")

# Elastic Net grid
enet_grid <- grid_regular(
  penalty(range = c(-4, 0)),  # 10^-4 to 10^0
  mixture(range = c(0, 1)),   # 0 = ridge, 1 = lasso
  levels = 5
)
cat("📊 Elastic Net grid:", nrow(enet_grid), "combinations\n")

# Random Forest grid
rf_grid <- grid_regular(
  mtry(range = c(2, 8)),
  min_n(range = c(5, 30)),
  levels = 5
)
cat("📊 Random Forest grid:", nrow(rf_grid), "combinations\n")

# XGBoost grid (smaller to manage computation)
xgb_grid <- grid_regular(
  trees(range = c(100, 500)),
  tree_depth(range = c(3, 8)),
  learn_rate(range = c(-3, -1)),  # 10^-3 to 10^-1
  min_n(range = c(5, 20)),
  levels = 3
)
cat("📊 XGBoost grid:", nrow(xgb_grid), "combinations\n\n")

# ----------------------------------------------------------------------------
# Step 7: Train Models
# ----------------------------------------------------------------------------

cat("STEP 7: Train Models (This may take several minutes...)\n")
cat("------------------------------------------------------------\n\n")

# Define metrics
reg_metrics <- metric_set(rmse, mae, rsq)

# --- Model 1: Linear Regression (no tuning, just fit with CV) ---
cat("🔄 Training Model 1: Linear Regression...\n")
tic <- Sys.time()

lm_results <- fit_resamples(
  lm_workflow,
  resamples = cv_folds,
  metrics = reg_metrics,
  control = control_resamples(save_pred = FALSE)
)

toc <- Sys.time()
cat("   ✓ Completed in", round(difftime(toc, tic, units = "secs"), 1), "seconds\n")

# --- Model 2: Elastic Net (tune penalty and mixture) ---
cat("🔄 Training Model 2: Elastic Net...\n")
tic <- Sys.time()

enet_results <- tune_grid(
  enet_workflow,
  resamples = cv_folds,
  grid = enet_grid,
  metrics = reg_metrics,
  control = control_grid(save_pred = FALSE)
)

toc <- Sys.time()
cat("   ✓ Completed in", round(difftime(toc, tic, units = "secs"), 1), "seconds\n")

# --- Model 3: Random Forest (tune mtry and min_n) ---
cat("🔄 Training Model 3: Random Forest...\n")
tic <- Sys.time()

rf_results <- tune_grid(
  rf_workflow,
  resamples = cv_folds,
  grid = rf_grid,
  metrics = reg_metrics,
  control = control_grid(save_pred = FALSE)
)

toc <- Sys.time()
cat("   ✓ Completed in", round(difftime(toc, tic, units = "secs"), 1), "seconds\n")

# --- Model 4: XGBoost (tune multiple hyperparameters) ---
cat("🔄 Training Model 4: XGBoost...\n")
tic <- Sys.time()

xgb_results <- tune_grid(
  xgb_workflow,
  resamples = cv_folds,
  grid = xgb_grid,
  metrics = reg_metrics,
  control = control_grid(save_pred = FALSE)
)

toc <- Sys.time()
cat("   ✓ Completed in", round(difftime(toc, tic, units = "secs"), 1), "seconds\n\n")

# ----------------------------------------------------------------------------
# Step 8: Compare Cross-Validation Results
# ----------------------------------------------------------------------------

cat("STEP 8: Compare Cross-Validation Results\n")
cat("------------------------------------------------------------\n\n")

# Get best results from each model
lm_best <- collect_metrics(lm_results) |>
  select(.metric, mean) |>
  pivot_wider(names_from = .metric, values_from = mean) |>
  mutate(model = "Linear Regression")

enet_best <- show_best(enet_results, metric = "rmse", n = 1) |>
  select(rmse = mean) |>
  bind_cols(
    show_best(enet_results, metric = "rsq", n = 1) |> select(rsq = mean),
    show_best(enet_results, metric = "mae", n = 1) |> select(mae = mean)
  ) |>
  mutate(model = "Elastic Net")

rf_best <- show_best(rf_results, metric = "rmse", n = 1) |>
  select(rmse = mean) |>
  bind_cols(
    show_best(rf_results, metric = "rsq", n = 1) |> select(rsq = mean),
    show_best(rf_results, metric = "mae", n = 1) |> select(mae = mean)
  ) |>
  mutate(model = "Random Forest")

xgb_best <- show_best(xgb_results, metric = "rmse", n = 1) |>
  select(rmse = mean) |>
  bind_cols(
    show_best(xgb_results, metric = "rsq", n = 1) |> select(rsq = mean),
    show_best(xgb_results, metric = "mae", n = 1) |> select(mae = mean)
  ) |>
  mutate(model = "XGBoost")

# Combine all results
comparison_table <- bind_rows(lm_best, enet_best, rf_best, xgb_best) |>
  select(model, rsq, rmse, mae) |>
  arrange(desc(rsq))

cat("📊 Cross-Validation Results (10-fold, TRAINING data only):\n\n")
print(comparison_table)

# Save comparison
write.csv(comparison_table, here("app", "models", "model_comparison.csv"), row.names = FALSE)
cat("\n✅ Model comparison saved: app/models/model_comparison.csv\n")

# ----------------------------------------------------------------------------
# Step 9: Select Best Model
# ----------------------------------------------------------------------------

cat("\n")
cat("STEP 9: Select Best Model\n")
cat("------------------------------------------------------------\n\n")

# Select model with highest R²
best_model_name <- comparison_table$model[1]
cat("🏆 Best model based on R²:", best_model_name, "\n")
cat("   R² =", round(comparison_table$rsq[1], 3), "\n")
cat("   RMSE =", round(comparison_table$rmse[1], 3), "\n")
cat("   MAE =", round(comparison_table$mae[1], 3), "\n")

# Get the best workflow
if (best_model_name == "Linear Regression") {
  best_workflow <- lm_workflow
  best_params <- NULL
} else if (best_model_name == "Elastic Net") {
  best_workflow <- enet_workflow
  best_params <- select_best(enet_results, metric = "rmse")
} else if (best_model_name == "Random Forest") {
  best_workflow <- rf_workflow
  best_params <- select_best(rf_results, metric = "rmse")
} else if (best_model_name == "XGBoost") {
  best_workflow <- xgb_workflow
  best_params <- select_best(xgb_results, metric = "rmse")
}

# Finalize workflow with best hyperparameters
if (!is.null(best_params)) {
  final_workflow <- finalize_workflow(best_workflow, best_params)
  cat("\n📊 Best hyperparameters:\n")
  print(best_params)
} else {
  final_workflow <- best_workflow
}

# ----------------------------------------------------------------------------
# Step 10: Final Model Training on Full Training Set
# ----------------------------------------------------------------------------

cat("\n")
cat("STEP 10: Final Model Training\n")
cat("------------------------------------------------------------\n\n")

cat("🔄 Training final model on full training set (", nrow(train_data), "trials, ≤", train_cutoff, ")...\n")
tic <- Sys.time()

final_fit <- fit(final_workflow, data = train_data)

toc <- Sys.time()
cat("   ✓ Final model trained in", round(difftime(toc, tic, units = "secs"), 1), "seconds\n")

# ----------------------------------------------------------------------------
# Step 11: Evaluate on Hold-Out Test Set (Later trials: 2016-2018)
# ----------------------------------------------------------------------------

cat("\n")
cat("STEP 11: Evaluate on Time-Based Test Set (", test_start, "-", test_end, ")\n")
cat("------------------------------------------------------------\n\n")

# Make predictions on test set
test_predictions <- predict(final_fit, test_data) |>
  bind_cols(test_data)

# Calculate test metrics
test_metrics <- test_predictions |>
  metrics(truth = log_duration, estimate = .pred) |>
  filter(.metric %in% c("rmse", "rsq", "mae"))

cat("📊 Test Set Performance (TEMPORAL HOLD-OUT: trials started", test_start, "-", test_end, "):\n\n")
print(test_metrics)

# Back-transform to months for interpretability
test_predictions <- test_predictions |>
  mutate(
    actual_months = exp(log_duration),
    predicted_months = exp(.pred),
    residual_months = actual_months - predicted_months,
    abs_error_months = abs(residual_months),
    pct_error = (residual_months / actual_months) * 100
  )

cat("\n📊 Prediction Accuracy in Months (Test Set):\n")
cat("   Mean Absolute Error:", round(mean(test_predictions$abs_error_months), 1), "months\n")
cat("   Median Absolute Error:", round(median(test_predictions$abs_error_months), 1), "months\n")
cat("   MAPE:", round(mean(abs(test_predictions$pct_error)), 1), "%\n")

# ----------------------------------------------------------------------------
# Step 12: Feature Importance
# ----------------------------------------------------------------------------

cat("\n")
cat("STEP 12: Feature Importance\n")
cat("------------------------------------------------------------\n\n")

# Extract fitted model
final_model <- extract_fit_parsnip(final_fit)

# Get feature importance (method depends on model type)
if (best_model_name %in% c("Random Forest", "XGBoost")) {
  
  importance_df <- vip::vi(final_model) |>
    arrange(desc(Importance))
  
  cat("📊 Top 10 Most Important Features:\n")
  print(head(importance_df, 10))
  
  # Save importance
  saveRDS(importance_df, here("app", "models", "feature_importance.rds"))
  
  # Plot
  p_importance <- importance_df |>
    slice_max(Importance, n = 10) |>
    ggplot(aes(x = reorder(Variable, Importance), y = Importance)) +
    geom_col(fill = "#0078D4", alpha = 0.8) +
    coord_flip() +
    labs(
      title = paste("Feature Importance -", best_model_name),
      subtitle = "Time-based validation (Stage 3)",
      x = "Feature",
      y = "Importance"
    ) +
    theme_minimal() +
    theme(plot.title = element_text(face = "bold"))
  
  ggsave(here("app", "docs", "10_feature_importance.png"), p_importance, width = 8, height = 6, dpi = 150)
  cat("\n📊 Saved: app/docs/10_feature_importance.png\n")
  
} else {
  # For linear models, use coefficients
  importance_df <- tidy(final_model) |>
    filter(term != "(Intercept)") |>
    mutate(abs_estimate = abs(estimate)) |>
    arrange(desc(abs_estimate))
  
  cat("📊 Top 10 Coefficient Magnitudes:\n")
  print(head(importance_df |> select(term, estimate, abs_estimate), 10))
  
  saveRDS(importance_df, here("app", "models", "feature_importance.rds"))
}

# ----------------------------------------------------------------------------
# Step 13: Diagnostic Plots
# ----------------------------------------------------------------------------

cat("\n")
cat("STEP 13: Diagnostic Plots\n")
cat("------------------------------------------------------------\n\n")

# Actual vs Predicted (log scale)
p1 <- ggplot(test_predictions, aes(x = log_duration, y = .pred)) +
  geom_point(alpha = 0.4, color = "#0078D4") +
  geom_abline(slope = 1, intercept = 0, color = "#D13438", linewidth = 1) +
  labs(
    title = "Actual vs Predicted (Log Scale)",
    subtitle = paste("Temporal Test Set (", test_start, "-", test_end, ") -", best_model_name),
    x = "Actual log(Duration)",
    y = "Predicted log(Duration)"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))

ggsave(here("app", "docs", "11_actual_vs_predicted_log.png"), p1, width = 8, height = 6, dpi = 150)
cat("📊 Saved: app/docs/11_actual_vs_predicted_log.png\n")

# Actual vs Predicted (months)
p2 <- ggplot(test_predictions, aes(x = actual_months, y = predicted_months)) +
  geom_point(alpha = 0.4, color = "#0078D4") +
  geom_abline(slope = 1, intercept = 0, color = "#D13438", linewidth = 1) +
  scale_x_continuous(limits = c(0, 200)) +
  scale_y_continuous(limits = c(0, 200)) +
  labs(
    title = "Actual vs Predicted (Months)",
    subtitle = paste("Temporal Test Set (", test_start, "-", test_end, ") -", best_model_name),
    x = "Actual Duration (months)",
    y = "Predicted Duration (months)"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))

ggsave(here("app", "docs", "12_actual_vs_predicted_months.png"), p2, width = 8, height = 6, dpi = 150)
cat("📊 Saved: app/docs/12_actual_vs_predicted_months.png\n")

# Residual plot
p3 <- ggplot(test_predictions, aes(x = .pred, y = log_duration - .pred)) +
  geom_point(alpha = 0.4, color = "#0078D4") +
  geom_hline(yintercept = 0, color = "#D13438", linewidth = 1) +
  labs(
    title = "Residual Plot",
    subtitle = paste("Temporal Test Set (", test_start, "-", test_end, ") -", best_model_name),
    x = "Predicted log(Duration)",
    y = "Residual (Actual - Predicted)"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))

ggsave(here("app", "docs", "13_residual_plot.png"), p3, width = 8, height = 6, dpi = 150)
cat("📊 Saved: app/docs/13_residual_plot.png\n")

# Residual distribution
p4 <- ggplot(test_predictions, aes(x = residual_months)) +
  geom_histogram(binwidth = 5, fill = "#0078D4", color = "white", alpha = 0.8) +
  geom_vline(xintercept = 0, color = "#D13438", linewidth = 1) +
  labs(
    title = "Residual Distribution (Months)",
    subtitle = paste("Temporal Test Set (", test_start, "-", test_end, ") -", best_model_name),
    x = "Residual (Actual - Predicted) in months",
    y = "Count"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))

ggsave(here("app", "docs", "14_residual_distribution.png"), p4, width = 8, height = 6, dpi = 150)
cat("📊 Saved: app/docs/14_residual_distribution.png\n")

# NEW: Temporal validation diagnostic - predictions by year
p5 <- test_predictions |>
  group_by(start_year) |>
  summarise(
    n = n(),
    median_error = median(residual_months),
    mean_error = mean(residual_months),
    mae = mean(abs_error_months),
    .groups = "drop"
  ) |>
  ggplot(aes(x = factor(start_year), y = mae)) +
  geom_col(fill = "#0078D4", alpha = 0.8) +
  geom_hline(yintercept = mean(test_predictions$abs_error_months), 
             color = "#D13438", linetype = "dashed", linewidth = 1) +
  labs(
    title = "Mean Absolute Error by Trial Start Year",
    subtitle = "Dashed line = overall MAE across test set",
    x = "Trial Start Year",
    y = "Mean Absolute Error (months)"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))

ggsave(here("app", "docs", "15_mae_by_year.png"), p5, width = 8, height = 5, dpi = 150)
cat("📊 Saved: app/docs/15_mae_by_year.png\n")

# ----------------------------------------------------------------------------
# Step 14: Save Final Model & Objects
# ----------------------------------------------------------------------------

cat("\n")
cat("STEP 14: Save Final Model & Objects\n")
cat("------------------------------------------------------------\n\n")

# Save final fitted workflow (for deployment)
saveRDS(final_fit, here("app", "models", "final_fit.rds"))
cat("✅ Final fit saved: app/models/final_fit.rds\n")

# Save the fitted workflow (final_fit IS the fitted workflow from fit())
# Note: extract_workflow() is for extracting from tune_results, not from fit() output
saveRDS(final_fit, here("app", "models", "final_workflow.rds"))
cat("✅ Final workflow saved: app/models/final_workflow.rds\n")

# Save test predictions
saveRDS(test_predictions, here("app", "models", "test_predictions.rds"))
write.csv(test_predictions, here("app", "models", "test_predictions.csv"), row.names = FALSE)
cat("✅ Test predictions saved: app/models/test_predictions.csv\n")

# Save temporal split info (NEW for Stage 3)
saveRDS(temporal_split_info, here("app", "models", "temporal_split_info.rds"))
cat("✅ Temporal split info saved: app/models/temporal_split_info.rds\n")

# Save all tuning results (for documentation)
tuning_results <- list(
  linear_regression = lm_results,
  elastic_net = enet_results,
  random_forest = rf_results,
  xgboost = xgb_results
)
saveRDS(tuning_results, here("app", "models", "all_tuning_results.rds"))
cat("✅ All tuning results saved: app/models/all_tuning_results.rds\n")

# ----------------------------------------------------------------------------
# Step 15: Generate Model Summary Report
# ----------------------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat("📋 MODEL SUMMARY REPORT - STAGE 3 (Time-Based Validation)\n")
cat("============================================================\n\n")

cat("VALIDATION APPROACH:\n")
cat("   Type: Time-based (train on past, test on held-out period)\n")
cat("   Training period: Trials started ≤", train_cutoff, "\n")
cat("   Test period: Trials started", test_start, "-", test_end, "\n")
cat("   Excluded: Trials >", test_end, "(right-truncation bias)\n")
cat("   Rationale: Avoids bias from incomplete recent trials\n")

cat("\nDATASET:\n")
cat("   Training samples:", nrow(train_data), "(trials ≤", train_cutoff, ")\n")
cat("   Test samples:", nrow(test_data), "(trials", test_start, "-", test_end, ")\n")
cat("   Excluded:", excluded_recent, "(trials >", test_end, ")\n")
cat("   Features:", length(trial_recipe$var_info$variable) - 1, "\n")
cat("   Target: log_duration (log-transformed months)\n")

cat("\nMODELS EVALUATED:\n")
cat("   1. Linear Regression (baseline)\n")
cat("   2. Elastic Net (regularized)\n")
cat("   3. Random Forest (ensemble)\n")
cat("   4. XGBoost (boosting)\n")

cat("\nCROSS-VALIDATION RESULTS (within training data):\n")
print(comparison_table)

cat("\nSELECTED MODEL:", best_model_name, "\n")

cat("\nTEST SET PERFORMANCE (TEMPORAL HOLD-OUT):\n")
print(test_metrics)

cat("\nPREDICTION ACCURACY (in months, on temporal test set):\n")
cat("   Mean Absolute Error:", round(mean(abs(test_predictions$residual_months)), 1), "months\n")
cat("   Median Absolute Error:", round(median(abs(test_predictions$residual_months)), 1), "months\n")
cat("   MAPE:", round(mean(abs(test_predictions$pct_error)), 1), "%\n")

# Extract R² for reporting
test_rsq <- test_metrics |> filter(.metric == "rsq") |> pull(.estimate)
test_rmse <- test_metrics |> filter(.metric == "rmse") |> pull(.estimate)
test_mae <- test_metrics |> filter(.metric == "mae") |> pull(.estimate)

cat("\n============================================================\n")
cat("⚠️  STAGE 3 VALIDATION NOTES:\n")
cat("============================================================\n")
cat("   Time-based validation avoids right-truncation bias:\n")
cat("   - Only completed trials are in the dataset\n
   - Recent trials (>2018) that finished quickly are over-represented\n")
cat("   - We exclude 2019+ trials and test on 2016-2018 instead\n")
cat("   - Both train and test sets have realistic duration distributions\n")
cat("============================================================\n")

cat("\n")
cat("============================================================\n")
cat("✅ Stage 3 Modeling Complete!\n")
cat("============================================================\n\n")

cat("📁 Files Created:\n")
cat("   app/models/final_fit.rds              (complete final fit object)\n")
cat("   app/models/final_workflow.rds         (fitted workflow for predictions)\n")
cat("   app/models/model_comparison.csv       (all model metrics)\n")
cat("   app/models/test_predictions.csv       (temporal test set predictions)\n")
cat("   app/models/feature_importance.rds     (feature importance scores)\n")
cat("   app/models/temporal_split_info.rds    (NEW: split documentation)\n")
cat("   app/models/all_tuning_results.rds     (full tuning history)\n")
cat("   app/docs/10_feature_importance.png\n")
cat("   app/docs/11_actual_vs_predicted_log.png\n")
cat("   app/docs/12_actual_vs_predicted_months.png\n")
cat("   app/docs/13_residual_plot.png\n")
cat("   app/docs/14_residual_distribution.png\n")
cat("   app/docs/15_mae_by_year.png           (NEW: temporal diagnostic)\n")

cat("\n🎯 Next Steps:\n")
cat("   1. Compare metrics to Stage 2.5 baseline (R² ~0.84)\n")
cat("   2. Update global.R model_metrics if changed significantly\n")
cat("   3. Update mod_methods.R to document validation approach\n")
cat("   4. Test prediction interface\n")

cat("\n============================================================\n")
cat("   Time-based validation provides more honest forecasting estimates!\n")
cat("============================================================\n")

# Print final metrics for easy copy to global.R
cat("\n📋 COPY TO global.R (model_metrics):\n")
cat("model_metrics <- list(\n")
cat("  r_squared =", round(test_rsq, 3), ",\n")
cat("  rmse =", round(test_rmse, 3), ",\n")
cat("  mae =", round(test_mae, 3), ",\n")
cat("  median_abs_error_months =", round(median(abs(test_predictions$residual_months)), 1), ",\n")
cat("  mean_abs_error_months =", round(mean(abs(test_predictions$residual_months)), 1), ",\n")
cat("  validation_type = \"time-based\",\n")
cat("  train_cutoff =", train_cutoff, ",\n")
cat("  test_start =", test_start, ",\n")
cat("  test_end =", test_end, ",\n")
cat("  train_n =", nrow(train_data), ",\n")
cat("  test_n =", nrow(test_data), "\n")
cat(")\n")