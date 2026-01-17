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

# Load trained Random Forest workflow
final_workflow <- readRDS("data/processed/final_workflow.rds")

# Load feature data (for reference distributions and validation)
trials_data <- readRDS("data/processed/trials_features.rds")

# Load feature importance
feature_importance <- readRDS("data/processed/feature_importance.rds")

# Load model comparison results
model_comparison <- read.csv("data/processed/model_comparison.csv")

# Load test predictions for diagnostic plots
test_predictions <- read.csv("data/processed/test_predictions.csv")

# -----------------------------------------------------------------------------
# Reference Statistics (for prediction context)
# -----------------------------------------------------------------------------
ref_stats <- list(
  # Duration statistics (in months)
  duration_median = median(trials_data$duration_months, na.rm = TRUE),
  duration_mean = mean(trials_data$duration_months, na.rm = TRUE),
  duration_q25 = quantile(trials_data$duration_months, 0.25, na.rm = TRUE),
  duration_q75 = quantile(trials_data$duration_months, 0.75, na.rm = TRUE),
  
  
  enrollment_median = median(trials_data$enrollment, na.rm = TRUE),
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
# Model Metrics (from training)
# -----------------------------------------------------------------------------
model_metrics <- list(
  r_squared = 0.790,
  rmse = 0.652,
  mae = 0.486,
  median_abs_error_months = 9.6,
  mean_abs_error_months = 18.1
)

# -----------------------------------------------------------------------------
# Helper: Predict Duration
# -----------------------------------------------------------------------------
predict_duration <- function(enrollment, n_sites, n_countries, 
                             registration_to_start, phase, sponsor) {
  
  # Build input tibble with all required features
  
  new_trial <- tibble::tibble(
    log_enrollment = log(enrollment),
    log_sites = log(n_sites + 1),
    n_countries = as.integer(n_countries),
    log_enrollment_per_site = log(enrollment / n_sites + 1),
    registration_to_start = as.numeric(registration_to_start),
    phase_simple = factor(phase, levels = phase_levels),
    sponsor_simple = factor(sponsor, levels = sponsor_levels),
    is_multisite = as.integer(n_sites > 1),
    is_multinational = as.integer(n_countries > 1)
  )
  
  # Predict (returns log_duration)
  pred <- predict(final_workflow, new_trial)
  
  # Convert back to months
  predicted_months <- exp(pred$.pred)
  
  return(predicted_months)
}

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
  version = "1.0.0",
  data_source = "ClinicalTrials.gov",
  n_trials = nrow(trials_data),
  therapeutic_area = "Oncology",
  phases = "Phase 2, Phase 2/3, Phase 3",
  model_type = "Random Forest"
)

cat("✓ Global setup complete\n")
cat("  - Model loaded:", class(final_workflow)[1], "\n")
cat("  - Trials loaded:", nrow(trials_data), "\n")
cat("  - Ready for predictions\n")