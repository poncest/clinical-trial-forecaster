# =============================================================================
# Clinical Trial Duration Forecaster - Global Setup
# =============================================================================
# This file loads packages, data, and models before the app starts
# All objects created here are available to ui.R and server.R
# =============================================================================

# -----------------------------------------------------------------------------
# Load Packages (explicit imports, no tidyverse meta-package)
# -----------------------------------------------------------------------------
library(shiny)
library(shiny.semantic)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(ggiraph)
library(reactable)
library(reactablefmtr)
library(workflows)
library(parsnip)
library(recipes)

# -----------------------------------------------------------------------------
# Color Palette - Clinical/Medical Theme
# -----------------------------------------------------------------------------
colors <- list(
  
  primary = "#0077B6",       
  primary_dark = "#023E8A", 
  primary_light = "#48CAE4",
  
  
  accent = "#006D77",        
  accent_light = "#83C5BE", 
  success = "#2E7D32",       
  warning = "#ED6C02",       
  
  text_primary = "#1A1A2E",
  text_secondary = "#4A4A68",
  
  text_muted = "#6B7280",
  
  background = "#F8FAFC",
  card_bg = "#FFFFFF",
  border = "#E2E8F0"
)

# -----------------------------------------------------------------------------
# Load Model and Data
# -----------------------------------------------------------------------------
# Stage 2.5: Paths updated to match pipeline output (app/models/, app/data-raw/)
message("Loading data files...")

final_workflow <- readRDS("models/final_workflow.rds")
trials_data <- readRDS("data-raw/modeling/trials_features.rds")
feature_importance <- readRDS("models/feature_importance.rds")
model_comparison <- read.csv("models/model_comparison.csv")
test_predictions <- read.csv("models/test_predictions.csv")

message("Data loaded.")

# -----------------------------------------------------------------------------
# Reference Statistics (for prediction context)
# -----------------------------------------------------------------------------

# Defensive check for enrollment field name (could be 'enrollment' or 'enrollment_count')
enrollment_col <- if ("enrollment" %in% names(trials_data)) {
  trials_data$enrollment
} else if ("enrollment_count" %in% names(trials_data)) {
  trials_data$enrollment_count
} else {
  warning("Enrollment column not found - using NA for reference stats")
  NA
}

ref_stats <- list(
  # Duration statistics (in months)
  duration_median = median(trials_data$duration_months, na.rm = TRUE),
  duration_mean = mean(trials_data$duration_months, na.rm = TRUE),
  duration_q25 = quantile(trials_data$duration_months, 0.25, na.rm = TRUE),
  duration_q75 = quantile(trials_data$duration_months, 0.75, na.rm = TRUE),
  
  # Enrollment (using defensive column check)
  enrollment_median = median(enrollment_col, na.rm = TRUE),
  sites_median = median(trials_data$n_sites, na.rm = TRUE),
  countries_median = median(trials_data$n_countries, na.rm = TRUE),
  reg_to_start_median = median(trials_data$registration_to_start, na.rm = TRUE),
  
  n_trials = nrow(trials_data),
  
  # Phase distribution
  phase_counts = table(trials_data$phase_simple),
  sponsor_counts = table(trials_data$sponsor_simple)
)

# Factor levels (needed for prediction compatibility)
phase_levels <- levels(trials_data$phase_simple)
sponsor_levels <- levels(trials_data$sponsor_simple)

# -----------------------------------------------------------------------------
# Model Metrics (from training - XGBoost, Stage 2.5 Option A)
# -----------------------------------------------------------------------------
model_metrics <- list(
  r_squared = 0.841,
  rmse = 0.570,
  mae = 0.430,
  median_abs_error_months = 7.4,
  mean_abs_error_months = 16.9
)

# -----------------------------------------------------------------------------
# DEPRECATED: predict_duration function removed in Stage 2.5
# -----------------------------------------------------------------------------
# The mod_forecaster.R module now calls predict(final_workflow, new_data)
# directly, using proper feature engineering that matches the training pipeline.
# This eliminates the schema mismatch risk from the old linear approximation.
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# Helper: Format Duration for Display
# -----------------------------------------------------------------------------
format_duration <- function(months) {
  if (months < 12) {
    return(paste0(round(months, 1), " months"))
  } else {
    years <- floor(months / 12)
    remaining_months <- round(months %% 12, 0)
    if (remaining_months == 0) {
      return(paste0(years, " year", ifelse(years > 1, "s", "")))
    } else {
      return(paste0(years, " year", ifelse(years > 1, "s", ""), ", ", 
                    remaining_months, " month", ifelse(remaining_months > 1, "s", "")))
    }
  }
}

# -----------------------------------------------------------------------------
# App Info
# -----------------------------------------------------------------------------
app_info <- list(
  title = "Clinical Trial Duration Forecaster",
  subtitle = "Predictive Analytics for Trial Planning",
  version = "1.2.0",  # Stage 2.5: Real model integration (Option A architecture)
  data_source = "ClinicalTrials.gov",
  n_trials = nrow(trials_data),
  therapeutic_area = "Oncology",
  phases = "Phase 2, Phase 2/3, Phase 3",
  model_type = "XGBoost",
  data_pulled = "January 2026",
  model_trained = "January 2026",
  last_updated = "January 2026",
  
  
  # Stage 2: Explicit inclusion criteria
  inclusion_criteria = list(
    study_type = "Interventional trials only",
    primary_purpose = "Treatment trials only",
    enrollment_type = "ACTUAL (post-completion final enrollment)",
    status = "Completed trials only"
  )
)

cat("✓ Global setup complete\n")
cat("  - Model loaded:", class(final_workflow)[1], "\n")
cat("  - Trials loaded:", nrow(trials_data), "\n")
cat("  - Ready for predictions\n")