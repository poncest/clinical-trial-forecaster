# ============================================================================
# 04_modeling.R
# Clinical Trial Forecaster - Model Training & Evaluation
# ============================================================================
# Purpose: Train, tune, compare, and select best forecasting model
# Author:  Steven Ponce
# Date:    January 2026
# ============================================================================

# ----------------------------------------------------------------------------
# Setup
# ----------------------------------------------------------------------------
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

# Create output directories
dir_create("models")
dir_create("docs")

# Tidymodels settings
tidymodels_prefer()  # Resolve function conflicts in favor of tidymodels

cat("\n")
cat("============================================================\n")
cat("🤖 Model Training & Evaluation\n")
cat("============================================================\n")
cat("Clinical Trial Duration Forecasting\n")
cat("============================================================\n\n")

# ----------------------------------------------------------------------------
# Step 1: Load Data & Recipe
# ----------------------------------------------------------------------------

cat("STEP 1: Load Data & Recipe\n")
cat("------------------------------------------------------------\n\n")

trials_features <- readRDS("data-raw/modeling/trials_features.rds")
trial_recipe <- readRDS("data-raw/modeling/trial_recipe.rds")

cat("📊 Loaded", nrow(trials_features), "trials with", ncol(trials_features), "features\n")
cat("📊 Target: log_duration (log-transformed trial duration in months)\n\n")

# ----------------------------------------------------------------------------
# Step 2: Train/Test Split
# ----------------------------------------------------------------------------

cat("STEP 2: Train/Test Split\n")
cat("------------------------------------------------------------\n\n")

# Stratified split by phase to ensure balanced representation
data_split <- initial_split(
  trials_features, 
  prop = 0.80, 
  strata = phase_simple
)

train_data <- training(data_split)
test_data <- testing(data_split)

cat("📊 Training set:", nrow(train_data), "trials (80%)\n")
cat("📊 Test set:", nrow(test_data), "trials (20%)\n")

# Verify stratification
cat("\n📊 Phase distribution check:\n")
cat("   Training:\n")
print(prop.table(table(train_data$phase_simple)) |> round(3))
cat("   Test:\n")
print(prop.table(table(test_data$phase_simple)) |> round(3))

# ----------------------------------------------------------------------------
# Step 3: Cross-Validation Folds
# ----------------------------------------------------------------------------

cat("\n")
cat("STEP 3: Cross-Validation Setup\n")
cat("------------------------------------------------------------\n\n")

# 10-fold CV, stratified by phase
cv_folds <- vfold_cv(
  train_data, 
  v = 10, 
  strata = phase_simple
)

cat("📊 Created 10-fold cross-validation\n")
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
  control = control_resamples(save_pred = TRUE)
)

toc <- Sys.time()
cat("   ✅ Complete in", round(difftime(toc, tic, units = "secs"), 1), "seconds\n\n")

# --- Model 2: Elastic Net (with tuning) ---
cat("🔄 Training Model 2: Elastic Net (tuning", nrow(enet_grid), "combinations)...\n")
tic <- Sys.time()

enet_results <- tune_grid(
  enet_workflow,
  resamples = cv_folds,
  grid = enet_grid,
  metrics = reg_metrics,
  control = control_grid(save_pred = TRUE)
)

toc <- Sys.time()
cat("   ✅ Complete in", round(difftime(toc, tic, units = "mins"), 1), "minutes\n\n")

# --- Model 3: Random Forest (with tuning) ---
cat("🔄 Training Model 3: Random Forest (tuning", nrow(rf_grid), "combinations)...\n")
tic <- Sys.time()

rf_results <- tune_grid(
  rf_workflow,
  resamples = cv_folds,
  grid = rf_grid,
  metrics = reg_metrics,
  control = control_grid(save_pred = TRUE)
)

toc <- Sys.time()
cat("   ✅ Complete in", round(difftime(toc, tic, units = "mins"), 1), "minutes\n\n")

# --- Model 4: XGBoost (with tuning) ---
cat("🔄 Training Model 4: XGBoost (tuning", nrow(xgb_grid), "combinations)...\n")
tic <- Sys.time()

xgb_results <- tune_grid(
  xgb_workflow,
  resamples = cv_folds,
  grid = xgb_grid,
  metrics = reg_metrics,
  control = control_grid(save_pred = TRUE)
)

toc <- Sys.time()
cat("   ✅ Complete in", round(difftime(toc, tic, units = "mins"), 1), "minutes\n\n")

# ----------------------------------------------------------------------------
# Step 8: Extract Best Parameters
# ----------------------------------------------------------------------------

cat("STEP 8: Extract Best Parameters\n")
cat("------------------------------------------------------------\n\n")

# Best Elastic Net
best_enet <- select_best(enet_results, metric = "rmse")
cat("📊 Best Elastic Net:\n")
print(best_enet)

# Best Random Forest
best_rf <- select_best(rf_results, metric = "rmse")
cat("\n📊 Best Random Forest:\n")
print(best_rf)

# Best XGBoost
best_xgb <- select_best(xgb_results, metric = "rmse")
cat("\n📊 Best XGBoost:\n")
print(best_xgb)

# ----------------------------------------------------------------------------
# Step 9: Compare All Models
# ----------------------------------------------------------------------------

cat("\n")
cat("STEP 9: Compare All Models (CV Performance)\n")
cat("------------------------------------------------------------\n\n")

# Collect best metrics from each model
lm_metrics <- collect_metrics(lm_results) |>
  mutate(model = "Linear Regression")

enet_metrics <- show_best(enet_results, metric = "rmse", n = 1) |>
  bind_rows(show_best(enet_results, metric = "mae", n = 1)) |>
  bind_rows(show_best(enet_results, metric = "rsq", n = 1)) |>
  select(.metric, mean, std_err) |>
  mutate(model = "Elastic Net")

rf_metrics <- show_best(rf_results, metric = "rmse", n = 1) |>
  bind_rows(show_best(rf_results, metric = "mae", n = 1)) |>
  bind_rows(show_best(rf_results, metric = "rsq", n = 1)) |>
  select(.metric, mean, std_err) |>
  mutate(model = "Random Forest")

xgb_metrics <- show_best(xgb_results, metric = "rmse", n = 1) |>
  bind_rows(show_best(xgb_results, metric = "mae", n = 1)) |>
  bind_rows(show_best(xgb_results, metric = "rsq", n = 1)) |>
  select(.metric, mean, std_err) |>
  mutate(model = "XGBoost")

# Combine all
all_metrics <- bind_rows(
  lm_metrics |> select(.metric, mean, std_err, model),
  enet_metrics,
  rf_metrics,
  xgb_metrics
)

# Create comparison table
comparison_table <- all_metrics |>
  select(model, .metric, mean) |>
  pivot_wider(names_from = .metric, values_from = mean) |>
  arrange(rmse)

cat("📊 Model Comparison (CV Performance):\n\n")
print(comparison_table)

# Save comparison table
saveRDS(comparison_table, "models/model_comparison.rds")
write.csv(comparison_table, "models/model_comparison.csv", row.names = FALSE)

# ----------------------------------------------------------------------------
# Step 10: Select Best Model
# ----------------------------------------------------------------------------

cat("\n")
cat("STEP 10: Select Best Model\n")
cat("------------------------------------------------------------\n\n")

# Find model with lowest RMSE
best_model_name <- comparison_table |>
  slice_min(rmse, n = 1) |>
  pull(model)

cat("🏆 Best Model (lowest CV RMSE):", best_model_name, "\n\n")

# Finalize the best workflow
if (best_model_name == "Linear Regression") {
  final_workflow <- lm_workflow
  cat("📋 Linear Regression selected - no tuning parameters\n")
  
} else if (best_model_name == "Elastic Net") {
  final_workflow <- finalize_workflow(enet_workflow, best_enet)
  cat("📋 Elastic Net finalized with:\n")
  print(best_enet)
  
} else if (best_model_name == "Random Forest") {
  final_workflow <- finalize_workflow(rf_workflow, best_rf)
  cat("📋 Random Forest finalized with:\n")
  print(best_rf)
  
} else if (best_model_name == "XGBoost") {
  final_workflow <- finalize_workflow(xgb_workflow, best_xgb)
  cat("📋 XGBoost finalized with:\n")
  print(best_xgb)
}

# ----------------------------------------------------------------------------
# Step 11: Final Evaluation on Test Set
# ----------------------------------------------------------------------------

cat("\n")
cat("STEP 11: Final Evaluation on Test Set\n")
cat("------------------------------------------------------------\n\n")

# Fit on full training data, evaluate on test data
final_fit <- last_fit(
  final_workflow,
  split = data_split,
  metrics = reg_metrics
)

# Extract test metrics
test_metrics <- collect_metrics(final_fit)
cat("📊 Test Set Performance:\n")
print(test_metrics)

# Get predictions
test_predictions <- collect_predictions(final_fit)

# Back-transform predictions (from log to months)
test_predictions <- test_predictions |>
  mutate(
    predicted_months = exp(.pred),
    actual_months = exp(log_duration),
    residual_months = actual_months - predicted_months,
    pct_error = (residual_months / actual_months) * 100
  )

cat("\n📊 Prediction Summary (in months):\n")
cat("   Mean Actual:", round(mean(test_predictions$actual_months), 1), "months\n")
cat("   Mean Predicted:", round(mean(test_predictions$predicted_months), 1), "months\n")
cat("   Mean Absolute Error:", round(mean(abs(test_predictions$residual_months)), 1), "months\n")
cat("   Median Absolute Error:", round(median(abs(test_predictions$residual_months)), 1), "months\n")
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
  saveRDS(importance_df, "models/feature_importance.rds")
  
  # Plot
  p_importance <- importance_df |>
    slice_max(Importance, n = 10) |>
    ggplot(aes(x = reorder(Variable, Importance), y = Importance)) +
    geom_col(fill = "#0078D4", alpha = 0.8) +
    coord_flip() +
    labs(
      title = paste("Feature Importance -", best_model_name),
      x = "Feature",
      y = "Importance"
    ) +
    theme_minimal() +
    theme(plot.title = element_text(face = "bold"))
  
  ggsave("docs/10_feature_importance.png", p_importance, width = 8, height = 6, dpi = 150)
  cat("\n📊 Saved: docs/10_feature_importance.png\n")
  
} else {
  # For linear models, use coefficients
  importance_df <- tidy(final_model) |>
    filter(term != "(Intercept)") |>
    mutate(abs_estimate = abs(estimate)) |>
    arrange(desc(abs_estimate))
  
  cat("📊 Top 10 Coefficient Magnitudes:\n")
  print(head(importance_df |> select(term, estimate, abs_estimate), 10))
  
  saveRDS(importance_df, "models/feature_importance.rds")
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
    subtitle = paste("Test Set -", best_model_name),
    x = "Actual log(Duration)",
    y = "Predicted log(Duration)"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))

ggsave("docs/11_actual_vs_predicted_log.png", p1, width = 8, height = 6, dpi = 150)
cat("📊 Saved: docs/11_actual_vs_predicted_log.png\n")

# Actual vs Predicted (months)
p2 <- ggplot(test_predictions, aes(x = actual_months, y = predicted_months)) +
  geom_point(alpha = 0.4, color = "#0078D4") +
  geom_abline(slope = 1, intercept = 0, color = "#D13438", linewidth = 1) +
  scale_x_continuous(limits = c(0, 200)) +
  scale_y_continuous(limits = c(0, 200)) +
  labs(
    title = "Actual vs Predicted (Months)",
    subtitle = paste("Test Set -", best_model_name),
    x = "Actual Duration (months)",
    y = "Predicted Duration (months)"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))

ggsave("docs/12_actual_vs_predicted_months.png", p2, width = 8, height = 6, dpi = 150)
cat("📊 Saved: docs/12_actual_vs_predicted_months.png\n")

# Residual plot
p3 <- ggplot(test_predictions, aes(x = .pred, y = log_duration - .pred)) +
  geom_point(alpha = 0.4, color = "#0078D4") +
  geom_hline(yintercept = 0, color = "#D13438", linewidth = 1) +
  labs(
    title = "Residual Plot",
    subtitle = paste("Test Set -", best_model_name),
    x = "Predicted log(Duration)",
    y = "Residual (Actual - Predicted)"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))

ggsave("docs/13_residual_plot.png", p3, width = 8, height = 6, dpi = 150)
cat("📊 Saved: docs/13_residual_plot.png\n")

# Residual distribution
p4 <- ggplot(test_predictions, aes(x = residual_months)) +
  geom_histogram(binwidth = 5, fill = "#0078D4", color = "white", alpha = 0.8) +
  geom_vline(xintercept = 0, color = "#D13438", linewidth = 1) +
  labs(
    title = "Residual Distribution (Months)",
    subtitle = paste("Test Set -", best_model_name),
    x = "Residual (Actual - Predicted) in months",
    y = "Count"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))

ggsave("docs/14_residual_distribution.png", p4, width = 8, height = 6, dpi = 150)
cat("📊 Saved: docs/14_residual_distribution.png\n")

# ----------------------------------------------------------------------------
# Step 14: Save Final Model & Objects
# ----------------------------------------------------------------------------

cat("\n")
cat("STEP 14: Save Final Model & Objects\n")
cat("------------------------------------------------------------\n\n")

# Save final fitted workflow (for deployment)
saveRDS(final_fit, "models/final_fit.rds")
cat("✅ Final fit saved: models/final_fit.rds\n")

# Extract and save just the model (lighter weight)
final_workflow_fitted <- extract_workflow(final_fit)
saveRDS(final_workflow_fitted, "models/final_workflow.rds")
cat("✅ Final workflow saved: models/final_workflow.rds\n")

# Save test predictions
saveRDS(test_predictions, "models/test_predictions.rds")
write.csv(test_predictions, "models/test_predictions.csv", row.names = FALSE)
cat("✅ Test predictions saved: models/test_predictions.csv\n")

# Save all tuning results (for documentation)
tuning_results <- list(
  linear_regression = lm_results,
  elastic_net = enet_results,
  random_forest = rf_results,
  xgboost = xgb_results
)
saveRDS(tuning_results, "models/all_tuning_results.rds")
cat("✅ All tuning results saved: models/all_tuning_results.rds\n")

# ----------------------------------------------------------------------------
# Step 15: Generate Model Summary Report
# ----------------------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat("📋 MODEL SUMMARY REPORT\n")
cat("============================================================\n\n")

cat("DATASET:\n")
cat("   Training samples:", nrow(train_data), "\n")
cat("   Test samples:", nrow(test_data), "\n")
cat("   Features:", length(trial_recipe$var_info$variable) - 1, "\n")
cat("   Target: log_duration (log-transformed months)\n")

cat("\nMODELS EVALUATED:\n")
cat("   1. Linear Regression (baseline)\n")
cat("   2. Elastic Net (regularized)\n")
cat("   3. Random Forest (ensemble)\n")
cat("   4. XGBoost (boosting)\n")

cat("\nCROSS-VALIDATION RESULTS:\n")
print(comparison_table)

cat("\nSELECTED MODEL:", best_model_name, "\n")

cat("\nTEST SET PERFORMANCE:\n")
print(test_metrics)

cat("\nPREDICTION ACCURACY (in months):\n")
cat("   Mean Absolute Error:", round(mean(abs(test_predictions$residual_months)), 1), "months\n")
cat("   Median Absolute Error:", round(median(abs(test_predictions$residual_months)), 1), "months\n")
cat("   MAPE:", round(mean(abs(test_predictions$pct_error)), 1), "%\n")

cat("\n============================================================\n")
cat("✅ Modeling Complete!\n")
cat("============================================================\n\n")

cat("📁 Files Created:\n")
cat("   models/final_fit.rds              (complete final fit object)\n")
cat("   models/final_workflow.rds         (fitted workflow for predictions)\n")
cat("   models/model_comparison.csv       (all model metrics)\n")
cat("   models/test_predictions.csv       (test set predictions)\n")
cat("   models/feature_importance.rds     (feature importance scores)\n")
cat("   models/all_tuning_results.rds     (full tuning history)\n")
cat("   docs/10_feature_importance.png\n")
cat("   docs/11_actual_vs_predicted_log.png\n")
cat("   docs/12_actual_vs_predicted_months.png\n")
cat("   docs/13_residual_plot.png\n")
cat("   docs/14_residual_distribution.png\n")

cat("\n🎯 Next Steps:\n")
cat("   1. Review model performance and diagnostic plots\n")
cat("   2. Copy final_workflow.rds to app/data/ for dashboard\n")
cat("   3. Build Shiny app modules\n")
cat("   4. Create prediction interface\n")

cat("\n============================================================\n")
cat("   Ready to build dashboard with trained model!\n")
cat("============================================================\n")