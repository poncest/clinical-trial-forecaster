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
library(dplyr)
library(tidyr)
library(stringr)
library(forcats)
library(lubridate)
library(ggplot2)
library(scales)
library(tidymodels)
library(fs)

# Create output directory
dir_create("data-raw/modeling")
dir_create("docs")

cat("\n")
cat("============================================================\n")
cat("🔧 Feature Engineering for Clinical Trial Forecaster\n")
cat("============================================================\n\n")

# ----------------------------------------------------------------------------
# Step 1: Load Clean Data
# ----------------------------------------------------------------------------

cat("STEP 1: Load Clean Data\n")
cat("------------------------------------------------------------\n\n")

trials_clean <- readRDS("data-raw/oncology_trials_clean.rds")

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
      sponsor_class == "OTHER" ~ "Academic/Other",
      sponsor_class == "NIH" ~ "NIH",
      sponsor_class %in% c("NETWORK", "OTHER_GOV", "FED") ~ "Government/Network",
      TRUE ~ "Other"
    ) |> factor(levels = c("Industry", "Academic/Other", "NIH", "Government/Network", "Other")),
    
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
    registration_to_start = as.numeric(start_date_parsed - ymd(first_submit_date))
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
# Step 6: Create Tidymodels Recipe
# ----------------------------------------------------------------------------

cat("\n")
cat("STEP 6: Create Tidymodels Recipe\n")
cat("------------------------------------------------------------\n\n")

# Define predictors for modeling
predictor_vars <- c(
  # Numeric (will be normalized)
  "log_enrollment",
  "log_sites", 
  "n_countries",
  "log_enrollment_per_site",
  "registration_to_start",
  
  # Categorical (will be dummy encoded)
  "phase_simple",
  "sponsor_simple",
  "is_multisite",
  "is_multinational"
)

# Create recipe
# Note: Recipe is created but not prepped - that happens during modeling
# This allows proper train/test handling

trial_recipe <- recipe(
  log_duration ~ log_enrollment + log_sites + n_countries + 
    log_enrollment_per_site + registration_to_start +
    phase_simple + sponsor_simple + is_multisite + is_multinational,
  data = trials_features
) |>
  # Convert logical to numeric FIRST (for glmnet/xgboost compatibility)
  step_mutate(
    is_multisite = as.integer(is_multisite),
    is_multinational = as.integer(is_multinational)
  ) |>
  # Handle missing values
  step_impute_median(all_numeric_predictors()) |>
  step_impute_mode(all_nominal_predictors()) |>
  
  # Normalize numeric predictors (z-score standardization)
  step_normalize(all_numeric_predictors()) |>
  
  # Create dummy variables for categoricals
  step_dummy(all_nominal_predictors(), one_hot = FALSE) |>
  
  # Remove zero-variance predictors
  step_zv(all_predictors())

cat("✅ Recipe created with steps:\n")
cat("   1. step_impute_median() - Handle missing numerics\n")
cat("   2. step_impute_mode() - Handle missing categoricals\n")
cat("   3. step_normalize() - Z-score standardization for numerics\n")
cat("   4. step_dummy() - One-hot encoding for categoricals\n")
cat("   5. step_zv() - Remove zero-variance predictors\n")

cat("\n📊 Target variable: log_duration (log-transformed months)\n")
cat("📊 Predictors:", length(predictor_vars), "features\n")

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

ggsave("docs/05_log_duration_distribution.png", p1, width = 8, height = 5, dpi = 150)
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

ggsave("docs/06_log_enrollment_vs_log_duration.png", p2, width = 8, height = 5, dpi = 150)
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

ggsave("docs/07_duration_by_sponsor.png", p3, width = 8, height = 5, dpi = 150)
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

ggsave("docs/08_duration_by_size.png", p4, width = 8, height = 5, dpi = 150)
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

ggsave("docs/09_duration_by_geography.png", p5, width = 8, height = 5, dpi = 150)
cat("📊 Saved: docs/09_duration_by_geography.png\n")

# ----------------------------------------------------------------------------
# Step 8: Save Outputs
# ----------------------------------------------------------------------------

cat("\n")
cat("STEP 8: Save Outputs\n")
cat("------------------------------------------------------------\n\n")

# Save feature-engineered dataset
saveRDS(trials_features, "data-raw/modeling/trials_features.rds")
cat("✅ Feature dataset saved: data-raw/modeling/trials_features.rds\n")

# Save recipe (unprepped - will be prepped during modeling)
saveRDS(trial_recipe, "data-raw/modeling/trial_recipe.rds")
cat("✅ Recipe saved: data-raw/modeling/trial_recipe.rds\n")

# Save feature metadata for documentation
feature_metadata <- tibble(
  feature = c(
    "log_enrollment", "log_sites", "n_countries", "log_enrollment_per_site",
    "registration_to_start", "phase_simple", "sponsor_simple", 
    "is_multisite", "is_multinational"
  ),
  type = c(
    "numeric", "numeric", "numeric", "numeric",
    "numeric", "categorical", "categorical",
    "binary", "binary"
  ),
  description = c(
    "Log of target enrollment count",
    "Log of number of sites + 1",
    "Number of countries",
    "Log of enrollment per site ratio",
    "Days from registration to trial start",
    "Simplified phase (Phase 2, Phase 3, Phase 1/2, Phase 2/3)",
    "Simplified sponsor type (Industry, Academic, NIH, Government)",
    "TRUE if more than 1 site",
    "TRUE if more than 1 country"
  ),
  preprocessing = c(
    "Normalize (z-score)", "Normalize (z-score)", "Normalize (z-score)", "Normalize (z-score)",
    "Normalize (z-score)", "Dummy encode", "Dummy encode",
    "Dummy encode", "Dummy encode"
  )
)

saveRDS(feature_metadata, "data-raw/modeling/feature_metadata.rds")
write.csv(feature_metadata, "data-raw/modeling/feature_metadata.csv", row.names = FALSE)
cat("✅ Feature metadata saved: data-raw/modeling/feature_metadata.csv\n")

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
cat("   data-raw/modeling/trials_features.rds    (modeling-ready dataset)\n")
cat("   data-raw/modeling/trial_recipe.rds       (tidymodels recipe)\n")
cat("   data-raw/modeling/feature_metadata.csv   (feature documentation)\n")
cat("   docs/05_log_duration_distribution.png\n")
cat("   docs/06_log_enrollment_vs_log_duration.png\n")
cat("   docs/07_duration_by_sponsor.png\n")
cat("   docs/08_duration_by_size.png\n")
cat("   docs/09_duration_by_geography.png\n")

cat("\n📋 Recipe Summary:\n")
cat("   Target: log_duration (log-transformed trial duration)\n
")
cat("   Predictors: 9 features\n")
cat("   Preprocessing: Imputation → Normalization → Dummy encoding → ZV removal\n")

cat("\n🎯 Next Steps:\n")
cat("   1. Run 04_modeling.R to train and evaluate models\n")
cat("   2. Compare baseline (linear) vs advanced (RF, XGBoost) models\n")
cat("   3. Select best model for dashboard\n")

cat("\n============================================================\n")
cat("   trials_features and trial_recipe objects available in environment\n")
cat("============================================================\n")