# ============================================================================
# 03_feature_engineering.R
# Clinical Trial Forecaster - Feature Engineering & Preprocessing
# ============================================================================
# Purpose: Create modeling-ready features with proper preprocessing
# Author:  Steven Ponce
# Date:    January 2026
# ============================================================================

# ----------------------------------------------------------------------------
# Setup
# ----------------------------------------------------------------------------
library(here)
library(dplyr)
library(tidyr)
library(stringr)
library(forcats)
library(lubridate)
library(ggplot2)
library(scales)
library(tidymodels)
library(fs)

# Create output directories (all outputs go to app/ folder)
dir_create(here("app", "data-raw", "modeling"))
dir_create(here("app", "docs"))

cat("\n")
cat("============================================================\n")
cat("🔧 Feature Engineering for Clinical Trial Forecaster\n")
cat("============================================================\n\n")

# ----------------------------------------------------------------------------
# Step 1: Load Clean Data
# ----------------------------------------------------------------------------

cat("STEP 1: Load Clean Data\n")
cat("------------------------------------------------------------\n\n")

trials_clean <- readRDS(here("app", "data-raw", "oncology_trials_clean.rds"))

cat("📊 Loaded", nrow(trials_clean), "trials with", ncol(trials_clean), "columns\n")
cat("📊 Duration range:", round(min(trials_clean$duration_months), 1), "-", 
    round(max(trials_clean$duration_months), 1), "months\n")
cat("📊 Enrollment range:", min(trials_clean$enrollment_count), "-", 
    max(trials_clean$enrollment_count), "patients\n\n")

# ----------------------------------------------------------------------------
# Step 2: Outlier Analysis & Handling
# ----------------------------------------------------------------------------

cat("STEP 2: Outlier Analysis & Handling\n")
cat("------------------------------------------------------------\n\n")

# Examine duration distribution
cat("📊 Duration percentiles:\n")
quantile(trials_clean$duration_months, probs = c(0, 0.01, 0.05, 0.25, 0.5, 0.75, 0.95, 0.99, 1)) |>
  round(1) |>
  print()

# Examine enrollment distribution
cat("\n📊 Enrollment percentiles:\n")
quantile(trials_clean$enrollment_count, probs = c(0, 0.01, 0.05, 0.25, 0.5, 0.75, 0.95, 0.99, 1)) |>
  round(0) |>
  print()

# Define outlier thresholds
duration_min <- 1      # Exclude trials < 1 month (likely data errors or very early terminations)
duration_max <- 180    # Exclude trials > 15 years (likely data errors)
enrollment_min <- 5    # Exclude trials < 5 patients (pilot studies)
enrollment_max <- 10000 # Exclude mega-trials (different dynamics)

cat("\n🔧 Applying outlier filters:\n")
cat("   Duration: ", duration_min, "-", duration_max, "months\n")
cat("   Enrollment: ", enrollment_min, "-", enrollment_max, "patients\n")

trials_filtered <- trials_clean |>
  filter(
    duration_months >= duration_min,
    duration_months <= duration_max,
    enrollment_count >= enrollment_min,
    enrollment_count <= enrollment_max
  )

cat("\n📊 Trials after outlier removal:", nrow(trials_filtered), 
    "(", round(nrow(trials_filtered)/nrow(trials_clean)*100, 1), "% retained)\n")

# ----------------------------------------------------------------------------
# Step 3: Create Derived Features
# ----------------------------------------------------------------------------

cat("\n")
cat("STEP 3: Create Derived Features\n")
cat("------------------------------------------------------------\n\n")

trials_features <- trials_filtered |>
  mutate(
    # --- LOG TRANSFORMS (for skewed distributions) ---
    log_enrollment = log(enrollment_count),
    log_sites = log(n_sites + 1),  # +1 to handle 0 sites
    log_duration = log(duration_months),  # Target transform for modeling
    
    # --- DERIVED RATIOS ---
    # Enrollment per site (measure of site burden)
    enrollment_per_site = case_when(
      n_sites > 0 ~ enrollment_count / n_sites,
      TRUE ~ enrollment_count  # Single-site trials
    ),
    log_enrollment_per_site = log(enrollment_per_site + 1),
    
    # --- BINARY FLAGS ---
    is_multisite = n_sites > 1,
    is_multinational = n_countries > 1,
    is_large_trial = enrollment_count >= 500,
    is_industry_sponsored = sponsor_class == "INDUSTRY",
    
    # --- CATEGORICAL SIMPLIFICATION ---
    # Phase: Collapse to 3 categories
    phase_simple = case_when(
      phase == "PHASE2" ~ "Phase 2",
      phase == "PHASE3" ~ "Phase 3",
      str_detect(phase, "PHASE1") ~ "Phase 1/2",
      str_detect(phase, "PHASE2, PHASE3") ~ "Phase 2/3",
      TRUE ~ "Other"
    ) |> factor(levels = c("Phase 2", "Phase 3", "Phase 1/2", "Phase 2/3", "Other")),
    
    # Sponsor: Collapse to 4 categories
    sponsor_simple = case_when(
      sponsor_class == "INDUSTRY" ~ "Industry",
      sponsor_class == "NIH" ~ "NIH",
      sponsor_class %in% c("NETWORK", "OTHER_GOV", "FED") ~ "Government/Network",
      # OTHER (mostly academic centers) and INDIV (individual investigators)
      TRUE ~ "Academic/Other"
    ) |> factor(levels = c("Industry", "Academic/Other", "NIH", "Government/Network")),
    
    # --- TRIAL SIZE CATEGORIES ---
    size_category = case_when(
      enrollment_count < 50 ~ "Small (<50)",
      enrollment_count < 200 ~ "Medium (50-199)",
      enrollment_count < 500 ~ "Large (200-499)",
      TRUE ~ "Very Large (500+)"
    ) |> factor(levels = c("Small (<50)", "Medium (50-199)", "Large (200-499)", "Very Large (500+)")),
    
    # --- SITE COMPLEXITY ---
    site_category = case_when(
      n_sites == 0 ~ "No sites listed",
      n_sites == 1 ~ "Single site",
      n_sites <= 10 ~ "2-10 sites",
      n_sites <= 50 ~ "11-50 sites",
      TRUE ~ "50+ sites"
    ) |> factor(levels = c("No sites listed", "Single site", "2-10 sites", "11-50 sites", "50+ sites")),
    
    # --- GEOGRAPHIC SCOPE ---
    geo_scope = case_when(
      n_countries == 0 ~ "No location data",
      n_countries == 1 ~ "Single country",
      n_countries <= 5 ~ "2-5 countries",
      n_countries <= 15 ~ "6-15 countries",
      TRUE ~ "Global (15+)"
    ) |> factor(levels = c("No location data", "Single country", "2-5 countries", "6-15 countries", "Global (15+)")),
    
    # --- TIME-BASED FEATURES ---
    # Year of trial start (for trend analysis)
    start_year = year(start_date_parsed),
    
    # Registration to start lag (days)
    registration_to_start = as.numeric(start_date_parsed - as.Date(first_submit_date))  # always a full date
  )

cat("✅ Created derived features:\n")
cat("   Log transforms: log_enrollment, log_sites, log_duration\n")
cat("   Ratios: enrollment_per_site, log_enrollment_per_site\n")
cat("   Binary flags: is_multisite, is_multinational, is_large_trial, is_industry_sponsored\n")
cat("   Categoricals: phase_simple, sponsor_simple, size_category, site_category, geo_scope\n")
cat("   Time features: start_year, registration_to_start\n")

# ----------------------------------------------------------------------------
# Step 4: Feature Summary Statistics
# ----------------------------------------------------------------------------

cat("\n")
cat("STEP 4: Feature Summary Statistics\n")
cat("------------------------------------------------------------\n\n")

# Numeric features summary
cat("📊 Numeric Features:\n\n")

numeric_summary <- trials_features |>
  select(duration_months, log_duration, enrollment_count, log_enrollment, 
         n_sites, log_sites, enrollment_per_site, n_countries) |>
  summary()
print(numeric_summary)

# Categorical distributions
cat("\n📊 Phase Distribution:\n")
trials_features |> count(phase_simple, sort = TRUE) |> print()

cat("\n📊 Sponsor Distribution:\n")
trials_features |> count(sponsor_simple, sort = TRUE) |> print()

cat("\n📊 Size Category Distribution:\n")
trials_features |> count(size_category) |> print()

cat("\n📊 Geographic Scope Distribution:\n")
trials_features |> count(geo_scope) |> print()

# ----------------------------------------------------------------------------
# Step 5: Correlation Analysis
# ----------------------------------------------------------------------------

cat("\n")
cat("STEP 5: Correlation Analysis\n")
cat("------------------------------------------------------------\n\n")

# Key correlations with duration
correlations <- trials_features |>
  select(duration_months, log_duration, enrollment_count, log_enrollment,
         n_sites, log_sites, n_countries, enrollment_per_site) |>
  cor(use = "complete.obs")

cat("📊 Correlations with duration_months:\n")
sort(correlations["duration_months", ], decreasing = TRUE) |>
  round(3) |>
  print()

cat("\n📊 Correlations with log_duration:\n")
sort(correlations["log_duration", ], decreasing = TRUE) |>
  round(3) |>
  print()

# ----------------------------------------------------------------------------
# Step 6: Create Tidymodels Recipe (Option A: Raw Inputs)
# ----------------------------------------------------------------------------

cat("\n")
cat("STEP 6: Create Tidymodels Recipe\n")
cat("------------------------------------------------------------\n\n")

# Define RAW predictors (transforms happen inside recipe)
# This is the "single source of truth" approach - app passes raw values,
# recipe handles all feature engineering identically to training.
predictor_vars <- c(
  # Raw numeric inputs
  "enrollment_count",
  "n_sites", 
  "n_countries",
  "registration_to_start",
  
  # Categorical (already factors)
  "phase_simple",
  "sponsor_simple"
)

# Create recipe with transforms INSIDE (Option A architecture)
# This eliminates schema drift risk between training and inference
trial_recipe <- recipe(
  log_duration ~ enrollment_count + n_sites + n_countries + 
    registration_to_start + phase_simple + sponsor_simple,
  data = trials_features
) |>
  
  # STEP 1: Create derived features from raw inputs
  # These transforms are now part of the recipe, not pre-computed
  step_mutate(
    # Log transforms (with offsets for numerical stability)
    log_enrollment = log(enrollment_count),
    log_sites = log(n_sites + 1),
    log_enrollment_per_site = log((enrollment_count / (n_sites + 1)) + 1),
    
    # Binary flags (as integer for model compatibility)
    is_multisite = as.integer(n_sites > 1),
    is_multinational = as.integer(n_countries > 1)
  ) |>
  
  # STEP 2: Remove raw inputs that have been transformed
  # Keep n_countries (used directly), registration_to_start (used directly)
  step_rm(enrollment_count, n_sites) |>
  
  # STEP 3: Handle missing values
  step_impute_median(all_numeric_predictors()) |>
  step_impute_mode(all_nominal_predictors()) |>
  
  # STEP 4: Normalize numeric predictors (z-score standardization)
  step_normalize(all_numeric_predictors()) |>
  
  # STEP 5: Create dummy variables for categoricals
  step_dummy(all_nominal_predictors(), one_hot = FALSE) |>
  
  # STEP 6: Remove zero-variance predictors
  step_zv(all_predictors())

cat("✅ Recipe created with Option A architecture (raw inputs):\n")
cat("   1. step_mutate() - Create log transforms and binary flags\n")
cat("   2. step_rm() - Remove raw inputs after transformation\n")
cat("   3. step_impute_median() - Handle missing numerics\n")
cat("   4. step_impute_mode() - Handle missing categoricals\n")
cat("   5. step_normalize() - Z-score standardization for numerics\n")
cat("   6. step_dummy() - One-hot encoding for categoricals\n")
cat("   7. step_zv() - Remove zero-variance predictors\n")

cat("\n📊 Target variable: log_duration (log-transformed months)\n")
cat("📊 Raw inputs:", length(predictor_vars), "features\n")
cat("📊 Derived features: log_enrollment, log_sites, log_enrollment_per_site, is_multisite, is_multinational\n")
cat("\n⚠️  IMPORTANT: App now passes raw values; recipe handles all transforms.\n")

# ----------------------------------------------------------------------------
# Step 7: Visualize Engineered Features
# ----------------------------------------------------------------------------

cat("\n")
cat("STEP 7: Visualize Engineered Features\n")
cat("------------------------------------------------------------\n\n")

# Log-transformed duration distribution
p1 <- ggplot(trials_features, aes(x = log_duration)) +
  geom_histogram(binwidth = 0.25, fill = "#0078D4", color = "white", alpha = 0.8) +
  labs(
    title = "Log-Transformed Duration Distribution",
    subtitle = "More normal distribution after log transform",
    x = "log(Duration in months)",
    y = "Count"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))

ggsave(here("app", "docs", "05_log_duration_distribution.png"), p1, width = 8, height = 5, dpi = 150)
cat("📊 Saved: docs/05_log_duration_distribution.png\n")

# Log enrollment vs log duration
p2 <- ggplot(trials_features, aes(x = log_enrollment, y = log_duration)) +
  geom_point(alpha = 0.3, color = "#0078D4") +
  geom_smooth(method = "lm", se = TRUE, color = "#D13438") +
  labs(
    title = "Log-Log Relationship: Enrollment vs Duration",
    subtitle = "More linear relationship after log transform",
    x = "log(Enrollment)",
    y = "log(Duration in months)"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))

ggsave(here("app", "docs", "06_log_enrollment_vs_log_duration.png"), p2, width = 8, height = 5, dpi = 150)
cat("📊 Saved: docs/06_log_enrollment_vs_log_duration.png\n")

# Duration by sponsor type
p3 <- ggplot(trials_features, aes(x = sponsor_simple, y = duration_months, fill = sponsor_simple)) +
  geom_boxplot(alpha = 0.8) +
  labs(
    title = "Trial Duration by Sponsor Type",
    x = "Sponsor Type",
    y = "Duration (months)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "none",
    axis.text.x = element_text(angle = 15, hjust = 1)
  )

ggsave(here("app", "docs", "07_duration_by_sponsor.png"), p3, width = 8, height = 5, dpi = 150)
cat("📊 Saved: docs/07_duration_by_sponsor.png\n")

# Duration by size category
p4 <- ggplot(trials_features, aes(x = size_category, y = duration_months, fill = size_category)) +
  geom_boxplot(alpha = 0.8) +
  labs(
    title = "Trial Duration by Trial Size",
    x = "Trial Size Category",
    y = "Duration (months)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "none"
  )

ggsave(here("app", "docs", "08_duration_by_size.png"), p4, width = 8, height = 5, dpi = 150)
cat("📊 Saved: docs/08_duration_by_size.png\n")

# Geographic scope vs duration
p5 <- ggplot(trials_features, aes(x = geo_scope, y = duration_months, fill = geo_scope)) +
  geom_boxplot(alpha = 0.8) +
  labs(
    title = "Trial Duration by Geographic Scope",
    x = "Geographic Scope",
    y = "Duration (months)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "none",
    axis.text.x = element_text(angle = 15, hjust = 1)
  )

ggsave(here("app", "docs", "09_duration_by_geography.png"), p5, width = 8, height = 5, dpi = 150)
cat("📊 Saved: docs/09_duration_by_geography.png\n")

# ----------------------------------------------------------------------------
# Step 8: Save Outputs
# ----------------------------------------------------------------------------

cat("\n")
cat("STEP 8: Save Outputs\n")
cat("------------------------------------------------------------\n\n")

# Save feature-engineered dataset
saveRDS(trials_features, here("app", "data-raw", "modeling", "trials_features.rds"))
cat("✅ Feature dataset saved: app/data-raw/modeling/trials_features.rds\n")

# Save recipe (unprepped - will be prepped during modeling)
saveRDS(trial_recipe, here("app", "data-raw", "modeling", "trial_recipe.rds"))
cat("✅ Recipe saved: app/data-raw/modeling/trial_recipe.rds\n")

# Save feature metadata for documentation (Option A: raw inputs → derived features)
feature_metadata <- tibble(
  feature = c(
    # Raw inputs (what app passes)
    "enrollment_count", "n_sites", "n_countries", "registration_to_start",
    "phase_simple", "sponsor_simple",
    # Derived features (computed by recipe)
    "log_enrollment", "log_sites", "log_enrollment_per_site",
    "is_multisite", "is_multinational"
  ),
  type = c(
    "raw_numeric", "raw_numeric", "raw_numeric", "raw_numeric",
    "raw_categorical", "raw_categorical",
    "derived_numeric", "derived_numeric", "derived_numeric",
    "derived_binary", "derived_binary"
  ),
  description = c(
    "Target enrollment count (raw input)",
    "Number of sites (raw input)",
    "Number of countries (raw input, used directly)",
    "Days from registration to trial start (raw input, used directly)",
    "Simplified phase (raw input)",
    "Simplified sponsor type (raw input)",
    "log(enrollment_count) - computed by recipe",
    "log(n_sites + 1) - computed by recipe",
    "log(enrollment_count / (n_sites + 1) + 1) - computed by recipe",
    "as.integer(n_sites > 1) - computed by recipe",
    "as.integer(n_countries > 1) - computed by recipe"
  ),
  preprocessing = c(
    "Input only (removed after transform)", "Input only (removed after transform)", 
    "Normalize (z-score)", "Normalize (z-score)",
    "Dummy encode", "Dummy encode",
    "Normalize (z-score)", "Normalize (z-score)", "Normalize (z-score)",
    "None (integer)", "None (integer)"
  )
)

saveRDS(feature_metadata, here("app", "data-raw", "modeling", "feature_metadata.rds"))
write.csv(feature_metadata, here("app", "data-raw", "modeling", "feature_metadata.csv"), row.names = FALSE)
cat("✅ Feature metadata saved: app/data-raw/modeling/feature_metadata.csv\n")
cat("   (Documents both raw inputs and derived features for Option A architecture)\n")

# ----------------------------------------------------------------------------
# Final Summary
# ----------------------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat("✅ Feature Engineering Complete!\n")
cat("============================================================\n\n")

cat("📊 Dataset Summary:\n")
cat("   Original trials: ", nrow(trials_clean), "\n")
cat("   After outlier removal: ", nrow(trials_features), "\n")
cat("   Features created: ", ncol(trials_features) - ncol(trials_clean), " new features\n")
cat("   Total columns: ", ncol(trials_features), "\n")

cat("\n📁 Files Created:\n")
cat("   app/data-raw/modeling/trials_features.rds    (modeling-ready dataset)\n")
cat("   app/data-raw/modeling/trial_recipe.rds       (tidymodels recipe)\n")
cat("   app/data-raw/modeling/feature_metadata.csv   (feature documentation)\n")
cat("   app/docs/05_log_duration_distribution.png\n")
cat("   app/docs/06_log_enrollment_vs_log_duration.png\n")
cat("   app/docs/07_duration_by_sponsor.png\n")
cat("   app/docs/08_duration_by_size.png\n")
cat("   app/docs/09_duration_by_geography.png\n")

cat("\n📋 Recipe Summary (Option A Architecture):\n")
cat("   Target: log_duration (log-transformed trial duration)\n")
cat("   Raw inputs: 6 features (enrollment_count, n_sites, n_countries,\n")
cat("               registration_to_start, phase_simple, sponsor_simple)\n")
cat("   Derived features: 5 (log_enrollment, log_sites, log_enrollment_per_site,\n")
cat("                     is_multisite, is_multinational)\n")
cat("   Preprocessing: Transforms → Remove raw → Impute → Normalize → Dummy → ZV\n")
cat("\n⚠️  App passes RAW inputs; recipe handles ALL transforms.\n")

cat("\n🎯 Next Steps:\n")
cat("   1. Run 04_modeling.R to train and evaluate models\n")
cat("   2. Compare baseline (linear) vs advanced (RF, XGBoost) models\n")
cat("   3. Model outputs go directly to app/models/\n")
cat("   4. App mod_forecaster.R passes raw inputs to predict()\n")

cat("\n============================================================\n")
cat("   trials_features and trial_recipe objects available in environment\n")
cat("============================================================\n")