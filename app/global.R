# =============================================================================
# Clinical Trial Duration Forecaster - Global Setup
# =============================================================================
# This file loads packages, data, and models before the app starts
# All objects created here are available to ui.R and server.R
# Time-based validation (train ≤2015, test 2016-2018); data rebuilt October 2026
# =============================================================================

# -----------------------------------------------------------------------------
# Load Packages (explicit imports, no tidyverse meta-package)
# -----------------------------------------------------------------------------

# Force dependency detection for shinyapps.io
if (FALSE) {
  library(shiny.semantic)
  library(semantic.dashboard)  
}

library(shiny)
library(shiny.semantic)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(ggiraph)
library(reactable)
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
  danger = "#D32F2F",
  
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
message("Loading data files...")

final_workflow <- readRDS("models/final_workflow.rds")
trials_data <- readRDS("data-raw/modeling/trials_features.rds")
feature_importance <- readRDS("models/feature_importance.rds")
model_comparison <- read.csv("models/model_comparison.csv")
test_predictions <- read.csv("models/test_predictions.csv")

# Temporal split info (cutoffs and counts written by scripts/04_modeling.R)
temporal_split_file <- "models/temporal_split_info.rds"
if (file.exists(temporal_split_file)) {
  temporal_split_info <- readRDS(temporal_split_file)
  message("Temporal split info loaded.")
} else {
  temporal_split_info <- NULL
}

message("Data loaded.")

# -----------------------------------------------------------------------------
# Reference Statistics (for prediction context)
# -----------------------------------------------------------------------------

# Defensive check for enrollment field name
enrollment_col <- if ("enrollment" %in% names(trials_data)) {
  trials_data$enrollment
} else if ("enrollment_count" %in% names(trials_data)) {
  trials_data$enrollment_count
} else {
  warning("Enrollment column not found - using NA for reference stats")
  NA
}

# Modeling cohort: trials that started by the end of the test window (2018).
# Trials starting 2019+ are excluded from modeling because only the shorter ones
# had completed by the January 2026 data pull, so including them would bias any
# "historical" duration baseline downward.
modeling_cutoff <- if (!is.null(temporal_split_info)) temporal_split_info$test_end else 2018
modeling_data <- trials_data[trials_data$start_year <= modeling_cutoff, ]

# Median duration by sponsor type in the modeling cohort (Executive Brief)
sponsor_medians <- round(tapply(modeling_data$duration_months, modeling_data$sponsor_simple, median))
phase_medians <- round(tapply(modeling_data$duration_months, modeling_data$phase_simple, median))

# Readable labels for model terms (dummy names use "." for "/" and spaces).
# Categorical terms name their reference level, since coefficients compare
# each level with it.
feature_label <- function(term) {
  labels <- c(
    registration_to_start = "Registration-to-Start Lag",
    log_sites = "Number of Sites (log)",
    log_enrollment = "Enrollment (log)",
    log_enrollment_per_site = "Enrollment per Site (log)",
    n_countries = "Number of Countries",
    is_multinational = "Multinational Flag",
    is_multisite = "Multi-site Flag",
    sponsor_simple_NIH = "Sponsor: NIH (vs Industry)",
    sponsor_simple_Government.Network = "Sponsor: Government/Network (vs Industry)",
    sponsor_simple_Academic.Other = "Sponsor: Academic/Other (vs Industry)",
    phase_simple_Phase.3 = "Phase 3 (vs Phase 2)",
    phase_simple_Phase.2.3 = "Phase 2/3 (vs Phase 2)",
    phase_simple_Phase.1.2 = "Phase 1/2 (vs Phase 2)"
  )
  out <- unname(labels[term])
  ifelse(is.na(out), gsub("_", " ", term), out)
}

ref_stats <- list(
  duration_median = median(modeling_data$duration_months, na.rm = TRUE),
  duration_mean = mean(modeling_data$duration_months, na.rm = TRUE),
  duration_q25 = quantile(modeling_data$duration_months, 0.25, na.rm = TRUE),
  duration_q75 = quantile(modeling_data$duration_months, 0.75, na.rm = TRUE),
  duration_values = modeling_data$duration_months,  # reference for percentiles
  baseline_n = nrow(modeling_data),
  baseline_cutoff = modeling_cutoff,

  # Empirical prediction band: log-scale residual quantiles (actual - predicted)
  # pooled over the whole 2016-2018 test set. Multiplying a prediction by exp(q)
  # gives a rough range; it is not conditional on the prediction, its 80%
  # coverage holds on that same test set by construction, and the upper end is
  # biased low because long test trials are missing (right-truncation).
  resid_q = quantile(test_predictions$log_duration - test_predictions$.pred,
                     c(0.10, 0.25, 0.75, 0.90), na.rm = TRUE),

  enrollment_median = median(enrollment_col, na.rm = TRUE),
  sites_median = median(trials_data$n_sites, na.rm = TRUE),
  countries_median = median(trials_data$n_countries, na.rm = TRUE),
  reg_to_start_median = median(modeling_data$registration_to_start, na.rm = TRUE),
  
  n_trials = nrow(trials_data),
  phase_counts = table(trials_data$phase_simple),
  sponsor_counts = table(trials_data$sponsor_simple)
)

# Factor levels (needed for prediction compatibility)
phase_levels <- levels(trials_data$phase_simple)
sponsor_levels <- levels(trials_data$sponsor_simple)

# -----------------------------------------------------------------------------
# Model Metrics (computed from the saved artifacts, never typed by hand)
# -----------------------------------------------------------------------------
# Time-based test set: trials started 2016-2018, model trained on <= 2015.
# R² is reported as 1 - SSE/SST on the log scale (can be negative: worse than
# predicting the test mean); squared correlation is reported separately.
# The random-split comparison comes from scripts/06_random_vs_time_split.R.
# -----------------------------------------------------------------------------

.y   <- test_predictions$log_duration
.p   <- test_predictions$.pred
.err <- test_predictions$actual_months - test_predictions$predicted_months  # actual - predicted
split_comparison <- read.csv("models/split_comparison.csv")
.rand <- split_comparison[split_comparison$split == "random (mean of 5)", ]
.rtrunc <- split_comparison[split_comparison$split == "random, truncation-matched (mean of 5)", ]
# Pre-start-only refit (scripts/08) and API return-order check (scripts/09)
prestart <- read.csv("models/prestart_comparison.csv")
prestart_r2 <- function(features, split) prestart$rsq_trad[prestart$features == features & prestart$split == split]
sampling_check <- read.csv("models/sampling_order_check.csv")
# Counts behind the October 2026 correction note (scripts/07_date_parsing_audit.R)
date_audit <- read.csv("models/date_parsing_audit.csv")
.spec <- workflows::extract_spec_parsnip(final_workflow)
# Models tied with the best on CV RMSE (within one SE of it), as in scripts/04
.best <- model_comparison[which.min(model_comparison$rmse), ]
tied_models <- model_comparison$model[model_comparison$rmse <= .best$rmse + .best$rmse_std_err]
all_models_tied <- length(tied_models) == nrow(model_comparison)
selection_note <- if (all_models_tied) {
  paste0("All ", nrow(model_comparison), " models are within one standard error of the best CV RMSE, ",
         "so they are effectively tied. Elastic Net was kept by a stated preference for a ",
         "regularized linear model with readable coefficients.")
} else {
  paste0(paste(tied_models, collapse = ", "), " are within one standard error of the best CV RMSE. ",
         "Elastic Net was kept by a stated preference for a regularized linear model with ",
         "readable coefficients.")
}
sensitivity_file <- "models/sensitivity_oncology_filter.csv"
sensitivity <- if (file.exists(sensitivity_file)) read.csv(sensitivity_file) else NULL

model_metrics <- list(
  # Time-based test performance
  r_squared = round(1 - sum((.y - .p)^2) / sum((.y - mean(.y))^2), 3),
  r_squared_cor = round(cor(.y, .p)^2, 3),
  rmse = round(sqrt(mean((.y - .p)^2)), 3),
  mae = round(mean(abs(.y - .p)), 3),
  median_abs_error_months = round(median(abs(.err)), 1),
  mean_abs_error_months = round(mean(abs(.err)), 1),
  median_error_months = round(median(.err), 1),
  share_overpredicted = round(mean(.err < 0), 2),

  # Random split, same cohort/recipe/grid (mean of 5 seeds)
  random_split_r_squared = round(.rand$rsq_trad, 2),
  random_split_r_squared_cor = round(.rand$rsq_cor, 2),
  random_split_median_abs_error_months = round(.rand$median_abs_err_mo, 1),
  random_split_rmse = round(.rand$rmse_log, 3),

  # Random split scored only on test trials no longer than the longest
  # time-based test trial: isolates the effect of the truncated test window
  random_trunc_r_squared = round(.rtrunc$rsq_trad, 2),
  random_trunc_r_squared_cor = round(.rtrunc$rsq_cor, 2),
  random_trunc_median_abs_error_months = round(.rtrunc$median_abs_err_mo, 1),

  # Level bias on the time-based test set (log scale, actual - predicted)
  mean_log_resid = round(mean(.y - .p), 3),
  pct_shorter_than_predicted = round(100 * (1 - exp(mean(.y - .p)))),

  # Share of the random-vs-time R² drop explained by the truncated window,
  # per random seed (min and max across the 5 splits)
  window_share_range = local({
    r  <- split_comparison$rsq_trad[grepl("^random \\(seed", split_comparison$split)]
    rt <- split_comparison$rsq_trad[grepl("^random, truncation-matched \\(seed", split_comparison$split)]
    tb <- split_comparison$rsq_trad[split_comparison$split == "time-based"]
    round(100 * range((r - rt) / (r - tb)) / 10) * 10
  }),

  # Fitted Elastic Net hyperparameters
  penalty = rlang::eval_tidy(.spec$args$penalty),
  mixture = rlang::eval_tidy(.spec$args$mixture),

  # Validation approach
  validation_type = "time-based",
  train_cutoff = temporal_split_info$train_cutoff,
  test_start = temporal_split_info$test_start,
  test_end = temporal_split_info$test_end,
  train_n = temporal_split_info$train_n,
  test_n = temporal_split_info$test_n,

  # Era medians (duration, months)
  train_median_months = round(median(trials_data$duration_months[trials_data$start_year <= temporal_split_info$train_cutoff]), 1),
  test_median_months = round(median(test_predictions$actual_months), 1),
  test_max_months = round(max(test_predictions$actual_months)),
  # Longest trial among the last test start year (tighter truncation)
  test_last_year_max_months = round(max(trials_data$duration_months[
    trials_data$start_year == temporal_split_info$test_end])),
  # Earlier trials no longer than the longest test trial (same truncation)
  train_capped_median_months = round(median(trials_data$duration_months[
    trials_data$start_year <= temporal_split_info$train_cutoff &
      trials_data$duration_months <= max(test_predictions$actual_months)]), 1)
)
rm(.y, .p, .err, .rand, .rtrunc, .spec, .best)
if (model_metrics$penalty <= 0.01) {
  selection_note <- paste0(selection_note, " At the small penalty CV chose (", model_metrics$penalty,
                           "), it is nearly identical to plain linear regression.")
}

# -----------------------------------------------------------------------------
# Helper: tidy inline text in UI trees
# -----------------------------------------------------------------------------
# htmltools writes each child of a tag on its own line, so p("R² = ", 0.02, ".")
# renders as "R² = 0.02 ." in the browser. tidy_text() walks a tag tree, merges
# runs of adjacent plain-text children, and drops the gap between a tag and
# punctuation that follows it (tags$strong("x"), "." -> "x."). Scripts, styles
# and HTML() strings are left untouched.
# -----------------------------------------------------------------------------
tidy_text <- function(x) {
  is_text <- function(c) is.atomic(c) && !is.null(c) && !inherits(c, "html") && length(c) == 1
  flatten <- function(ch) {
    out <- list()
    for (c in ch) {
      if (is.null(c)) next
      if (is.list(c) && !inherits(c, "shiny.tag") && !inherits(c, "html_dependency") &&
          is.null(attr(c, "html_dependencies")) && !inherits(c, "shiny.tag.list")) {
        out <- c(out, flatten(c))
      } else {
        out <- c(out, list(c))
      }
    }
    out
  }
  tidy_children <- function(ch) {
    ch <- flatten(ch)
    out <- list(); buf <- NULL
    for (c in ch) {
      if (is_text(c)) {
        buf <- paste0(if (is.null(buf)) "" else buf, as.character(c))
      } else {
        if (!is.null(buf)) { out <- c(out, list(buf)); buf <- NULL }
        out <- c(out, list(tidy_text(c)))
      }
    }
    if (!is.null(buf)) out <- c(out, list(buf))
    for (i in seq_along(out)) {
      if (inherits(out[[i]], "shiny.tag") && i < length(out) && is_text(out[[i + 1]]) &&
          grepl("^[.,;:!?)]", out[[i + 1]])) {
        out[[i]]$.noWS <- union(out[[i]]$.noWS, "after")
      }
    }
    out
  }
  if (inherits(x, "shiny.tag")) {
    if (tolower(x$name) %in% c("script", "style", "pre", "textarea")) return(x)
    x$children <- tidy_children(x$children)
    return(x)
  }
  if (inherits(x, "shiny.tag.list")) {
    attrs <- attributes(x)
    y <- lapply(x, tidy_text)
    attributes(y) <- attrs
    return(y)
  }
  x
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
  subtitle = "A Validation-First Case Study",
  version = "1.4.0",  # October 2026: date-parsing fix, retrained, metrics computed from artifacts
  data_source = "ClinicalTrials.gov",
  n_trials = nrow(trials_data),  # full cohort (includes 2019+ trials excluded from modeling)
  # Modeling split (time-based): train <= 2015, test 2016-2018
  n_train = if (!is.null(temporal_split_info)) temporal_split_info$train_n else NA_integer_,
  n_test  = if (!is.null(temporal_split_info)) temporal_split_info$test_n else NA_integer_,
  therapeutic_area = "Oncology",
  phases = "Phase 1/2, Phase 2, Phase 2/3, Phase 3",
  model_type = "Elastic Net",  # simplest model within 1 SE of the best CV RMSE
  data_pulled = "January 2026",
  model_trained = "October 2026",
  last_updated = "October 2026",
  
  # Inclusion criteria
  inclusion_criteria = list(
    study_type = "Interventional trials only",
    primary_purpose = "Treatment trials only",
    enrollment_type = "ACTUAL (post-completion final enrollment)",
    status = "Completed trials only"
  ),
  
  # Validation approach
  validation = list(
    type = "Time-based (train ≤2015, test 2016-2018)",
    rationale = "Avoids right-truncation bias from incomplete recent trials",
    key_finding = "Limited prospective forecasting capability from registry data alone"
  )
)

cat("✓ Global setup complete\n")
cat("  - Model loaded:", class(final_workflow)[1], "\n")
cat("  - Trials loaded:", nrow(trials_data), "\n")
cat("  - Validation type:", model_metrics$validation_type, "\n")
cat("  - R² (time-based):", model_metrics$r_squared, "\n")
cat("  - Ready for predictions (with documented limitations)\n")