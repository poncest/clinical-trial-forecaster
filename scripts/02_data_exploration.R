# ============================================================================
# 02_data_exploration.R
# Clinical Trial Forecaster - Data Exploration & Extraction
# ============================================================================
# Purpose: Query oncology trials, extract fields, assess data quality
# Author:  Steven Ponce
# Date:    January 2026
# ============================================================================

# ----------------------------------------------------------------------------
# Setup
# ----------------------------------------------------------------------------
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(lubridate)
library(janitor)
library(httr2)
library(jsonlite)
library(fs)
library(tictoc)
library(ggplot2)
library(scales)

# Create output directories
dir_create("data-raw/cache")
dir_create("docs")

# Null coalescing operator
`%||%` <- function(x, y) if (is.null(x)) y else x

# ----------------------------------------------------------------------------
# API Query Function (Enhanced)
# ----------------------------------------------------------------------------

base_url <- "https://clinicaltrials.gov/api/v2/studies"

query_clinicaltrials <- function(
    condition = NULL,
    phase = NULL,
    status = NULL,
    sponsor_type = NULL,
    page_size = 100,
    page_token = NULL,
    debug = FALSE
) {
  
  # Build base request
  req <- request(base_url) |>
    req_url_query(pageSize = page_size)
  
  # Add condition filter (simple term, no complex OR syntax)
  if (!is.null(condition)) {
    req <- req |> req_url_query(`query.cond` = condition)
  }
  
  # Add advanced filter string
  # API v2 uses filter.advanced for combined filters
  # Format: "AREA[Phase]PHASE2 AND AREA[OverallStatus]COMPLETED"
  filter_parts <- c()
  
  if (!is.null(phase)) {
    filter_parts <- c(filter_parts, sprintf("AREA[Phase]%s", phase))
  }
  
  if (!is.null(status)) {
    filter_parts <- c(filter_parts, sprintf("AREA[OverallStatus]%s", status))
  }
  
  if (length(filter_parts) > 0) {
    filter_string <- paste(filter_parts, collapse = " AND ")
    req <- req |> req_url_query(`filter.advanced` = filter_string)
  }
  
  # Add page token for pagination
  if (!is.null(page_token)) {
    req <- req |> req_url_query(pageToken = page_token)
  }
  
  # Debug: show URL
  if (debug) {
    cat("🔗 Request URL:", req$url, "\n")
  }
  
  # Timeout for slow connections
  req <- req |> req_timeout(120)
  
  # Perform request with error handling
  tryCatch({
    resp <- req |> req_perform()
    resp |> resp_body_json()
  }, error = function(e) {
    cat("❌ API Error:", conditionMessage(e), "\n")
    if (debug) {
      cat("   URL attempted:", req$url, "\n")
    }
    return(NULL)
  })
}

# ----------------------------------------------------------------------------
# Pagination Function - Get All Results
# ----------------------------------------------------------------------------

get_all_trials <- function(
    condition = NULL,
    phase = NULL,
    status = NULL,
    max_pages = 50  # Safety limit: 50 pages × 100 = 5,000 trials max
) {
  
  all_studies <- list()
  page_token <- NULL
  page_count <- 0
  
  cat("📥 Downloading trials...\n")
  
  repeat {
    page_count <- page_count + 1
    cat(sprintf("   Page %d...", page_count))
    
    result <- query_clinicaltrials(
      condition = condition,
      phase = phase,
      status = status,
      page_size = 100,
      page_token = page_token
    )
    
    if (is.null(result)) {
      cat(" ❌ Failed\n")
      break
    }
    
    n_studies <- length(result$studies)
    cat(sprintf(" %d trials\n", n_studies))
    
    all_studies <- c(all_studies, result$studies)
    
    # Check for next page
    page_token <- result$nextPageToken
    
    if (is.null(page_token) || page_count >= max_pages) {
      break
    }
    
    # Be nice to the API
    Sys.sleep(0.5)
  }
  
  cat(sprintf("\n✅ Total trials downloaded: %d\n", length(all_studies)))
  return(all_studies)
}

# ----------------------------------------------------------------------------
# Extract Fields from Single Trial
# ----------------------------------------------------------------------------

extract_trial_fields <- function(study) {
  
  ps <- study$protocolSection
  
  # Safely extract nested fields
  safe_extract <- function(x, default = NA_character_) {
    if (is.null(x)) default else x
  }
  
  # Extract location info
  locations <- ps$contactsLocationsModule$locations %||% list()
  n_sites <- length(locations)
  countries <- unique(sapply(locations, function(loc) safe_extract(loc$country)))
  countries <- countries[!is.na(countries)]
  n_countries <- length(countries)
  
  # Extract phases (can be multiple)
  phases <- ps$designModule$phases %||% list("NA")
  phase_str <- paste(unlist(phases), collapse = ", ")
  
  # Extract conditions
  conditions <- ps$conditionsModule$conditions %||% list()
  condition_str <- paste(unlist(conditions)[1:min(3, length(conditions))], collapse = "; ")
  
  # Build row
  tibble(
    # Identification
    nct_id = safe_extract(ps$identificationModule$nctId),
    brief_title = safe_extract(ps$identificationModule$briefTitle),
    sponsor_name = safe_extract(ps$sponsorCollaboratorsModule$leadSponsor$name),
    sponsor_class = safe_extract(ps$sponsorCollaboratorsModule$leadSponsor$class),
    
    # Status
    overall_status = safe_extract(ps$statusModule$overallStatus),
    
    # Dates
    start_date = safe_extract(ps$statusModule$startDateStruct$date),
    start_date_type = safe_extract(ps$statusModule$startDateStruct$type),
    completion_date = safe_extract(ps$statusModule$completionDateStruct$date),
    completion_date_type = safe_extract(ps$statusModule$completionDateStruct$type),
    primary_completion_date = safe_extract(ps$statusModule$primaryCompletionDateStruct$date),
    primary_completion_type = safe_extract(ps$statusModule$primaryCompletionDateStruct$type),
    first_submit_date = safe_extract(ps$statusModule$studyFirstSubmitDate),
    
    # Design
    study_type = safe_extract(ps$designModule$studyType),
    phase = phase_str,
    allocation = safe_extract(ps$designModule$designInfo$allocation),
    intervention_model = safe_extract(ps$designModule$designInfo$interventionModel),
    primary_purpose = safe_extract(ps$designModule$designInfo$primaryPurpose),
    masking = safe_extract(ps$designModule$designInfo$maskingInfo$masking),
    
    # Enrollment
    enrollment_count = safe_extract(ps$designModule$enrollmentInfo$count, default = NA_integer_),
    enrollment_type = safe_extract(ps$designModule$enrollmentInfo$type),
    
    # Conditions
    conditions = condition_str,
    
    # Locations
    n_sites = n_sites,
    n_countries = n_countries,
    countries = paste(countries, collapse = "; "),
    
    # Results
    has_results = safe_extract(study$hasResults, default = FALSE)
  )
}

# ----------------------------------------------------------------------------
# Extract All Trials to Data Frame
# ----------------------------------------------------------------------------

extract_all_trials <- function(studies) {
  
  cat("📊 Extracting fields from", length(studies), "trials...\n")
  
  df <- map_dfr(studies, possibly(extract_trial_fields, otherwise = NULL), .progress = TRUE)
  
  cat("✅ Extracted", nrow(df), "trials\n")
  return(df)
}

# ============================================================================
# MAIN EXECUTION
# ============================================================================

cat("\n")
cat("============================================================\n")
cat("🔬 Clinical Trial Data Exploration\n")
cat("============================================================\n")
cat("Focus: Oncology Phase 2/3 Completed Trials\n")
cat("============================================================\n\n")

# ----------------------------------------------------------------------------
# Step 0: Test API Connection
# ----------------------------------------------------------------------------

cat("STEP 0: Testing API Connection\n")
cat("------------------------------------------------------------\n\n")

test_result <- query_clinicaltrials(
  condition = "cancer",
  page_size = 5,
  debug = TRUE
)

if (is.null(test_result) || length(test_result$studies) == 0) {
  stop("❌ API test failed. Check internet connection and try again.")
} else {
  cat("✅ API connection successful! Retrieved", length(test_result$studies), "test trials.\n\n")
}

# ----------------------------------------------------------------------------
# Step 1: Download Oncology Trials
# ----------------------------------------------------------------------------

cat("STEP 1: Downloading Oncology Trials\n")
cat("------------------------------------------------------------\n\n")

tic("Download")

# Query for completed oncology Phase 2 and Phase 3 trials
# Using simple condition term that works with API

cat("📥 Downloading Phase 2 completed oncology trials...\n")
phase2_trials <- get_all_trials(
  condition = "cancer",
  phase = "PHASE2",
  status = "COMPLETED",
  max_pages = 20  # Limit for exploration (20 × 100 = 2,000 max)
)

cat("\n📥 Downloading Phase 3 completed oncology trials...\n")
phase3_trials <- get_all_trials(
  condition = "cancer",
  phase = "PHASE3",
  status = "COMPLETED",
  max_pages = 20
)

# Combine
all_trials <- c(phase2_trials, phase3_trials)

toc()

cat("\n📊 Summary:\n")
cat("   Phase 2 trials:", length(phase2_trials), "\n")
cat("   Phase 3 trials:", length(phase3_trials), "\n")
cat("   Total trials:", length(all_trials), "\n")

# Save raw data
saveRDS(all_trials, "data-raw/cache/oncology_trials_raw.rds")
cat("\n✅ Raw data saved to: data-raw/cache/oncology_trials_raw.rds\n")

# ----------------------------------------------------------------------------
# Step 2: Extract Fields to Data Frame
# ----------------------------------------------------------------------------

cat("\n")
cat("STEP 2: Extracting Fields\n")
cat("------------------------------------------------------------\n\n")

tic("Extraction")
trials_df <- extract_all_trials(all_trials)
toc()

# Save extracted data
saveRDS(trials_df, "data-raw/cache/oncology_trials_extracted.rds")
cat("\n✅ Extracted data saved to: data-raw/cache/oncology_trials_extracted.rds\n")

# ----------------------------------------------------------------------------
# Step 3: Data Quality Assessment
# ----------------------------------------------------------------------------

cat("\n")
cat("STEP 3: Data Quality Assessment\n")
cat("------------------------------------------------------------\n\n")

# Check if we have data
if (nrow(trials_df) == 0) {
  cat("⚠️  No trials extracted. Check API responses above.\n")
  cat("   Stopping script - need data to continue.\n")
  stop("No trials extracted from API")
}

# Completeness by field
completeness <- trials_df |>
  summarise(across(everything(), ~ mean(!is.na(.) & . != ""))) |>
  pivot_longer(everything(), names_to = "field", values_to = "pct_complete") |>
  arrange(desc(pct_complete))

cat("📊 Field Completeness:\n\n")
print(completeness, n = 30)

# ----------------------------------------------------------------------------
# Step 4: Calculate Target Variable (Enrollment Duration)
# ----------------------------------------------------------------------------

cat("\n")
cat("STEP 4: Calculate Enrollment Duration\n")
cat("------------------------------------------------------------\n\n")

trials_clean <- trials_df |>
  # Parse dates
  mutate(
    start_date_parsed = ymd(start_date),
    completion_date_parsed = ymd(completion_date),
    primary_completion_parsed = ymd(primary_completion_date)
  ) |>
  # Calculate duration in months
  mutate(
    duration_months = interval(start_date_parsed, completion_date_parsed) / months(1),
    duration_to_primary_months = interval(start_date_parsed, primary_completion_parsed) / months(1)
  ) |>
  # Filter for modeling (Stage 2 fixes applied)
  filter(
    # --- CRITICAL: Study type filter ---
    study_type == "INTERVENTIONAL",  # Exclude observational studies
    
    # --- Narrow to treatment trials ---
    primary_purpose == "TREATMENT",  # Exclude prevention, supportive care, screening
    
    # --- Duration validity ---
    !is.na(duration_months),
    duration_months > 0,
    duration_months < 240,  # Exclude outliers (> 20 years)
    
    # --- Enrollment validity (ACTUAL = post-completion final enrollment) ---
    enrollment_type == "ACTUAL",
    !is.na(enrollment_count),
    enrollment_count > 0
  )

cat("📊 Trials with Valid Duration Data:\n")
cat("   Original trials:", nrow(trials_df), "\n")
cat("   After filtering:", nrow(trials_clean), "\n")
cat("   Usable for modeling:", round(nrow(trials_clean) / nrow(trials_df) * 100, 1), "%\n")

# --- Stage 2: Detailed filter diagnostics ---
cat("\n📋 Filter Diagnostics (Stage 2):\n")
cat("   Interventional only:", sum(trials_df$study_type == "INTERVENTIONAL", na.rm = TRUE), "\n")
cat("   Treatment purpose:", sum(trials_df$primary_purpose == "TREATMENT", na.rm = TRUE), "\n")
cat("   ACTUAL enrollment:", sum(trials_df$enrollment_type == "ACTUAL", na.rm = TRUE), "\n")

# ----------------------------------------------------------------------------
# Step 5: Summary Statistics
# ----------------------------------------------------------------------------

cat("\n")
cat("STEP 5: Summary Statistics\n")
cat("------------------------------------------------------------\n\n")

cat("📊 Duration (months):\n")
summary(trials_clean$duration_months) |> print()

cat("\n📊 Enrollment Count:\n")
summary(trials_clean$enrollment_count) |> print()

cat("\n📊 Number of Sites:\n")
summary(trials_clean$n_sites) |> print()

cat("\n📊 Number of Countries:\n")
summary(trials_clean$n_countries) |> print()

cat("\n📊 By Phase:\n")
trials_clean |>
  count(phase) |>
  arrange(desc(n)) |>
  print()

cat("\n📊 By Sponsor Class:\n")
trials_clean |>
  count(sponsor_class) |>
  arrange(desc(n)) |>
  print()

# ----------------------------------------------------------------------------
# Step 6: Save Clean Dataset
# ----------------------------------------------------------------------------

cat("\n")
cat("STEP 6: Save Clean Dataset\n")
cat("------------------------------------------------------------\n\n")

saveRDS(trials_clean, "data-raw/oncology_trials_clean.rds")
cat("✅ Clean dataset saved to: data-raw/oncology_trials_clean.rds\n")

# Also save as CSV for inspection
write.csv(trials_clean, "data-raw/oncology_trials_clean.csv", row.names = FALSE)
cat("✅ CSV version saved to: data-raw/oncology_trials_clean.csv\n")

# ----------------------------------------------------------------------------
# Step 7: Quick Visualizations
# ----------------------------------------------------------------------------

cat("\n")
cat("STEP 7: Exploratory Visualizations\n")
cat("------------------------------------------------------------\n\n")

# Duration distribution
p1 <- ggplot(trials_clean, aes(x = duration_months)) +
  geom_histogram(binwidth = 6, fill = "#0078D4", color = "white", alpha = 0.8) +
  labs(
    title = "Distribution of Trial Duration",
    subtitle = paste0("Oncology Phase 2/3 Completed Trials (n = ", nrow(trials_clean), ")"),
    x = "Duration (months)",
    y = "Count"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))

ggsave("docs/01_duration_distribution.png", p1, width = 8, height = 5, dpi = 150)
cat("📊 Saved: docs/01_duration_distribution.png\n")

# Duration by phase
p2 <- ggplot(trials_clean, aes(x = phase, y = duration_months, fill = phase)) +
  geom_boxplot(alpha = 0.8) +
  labs(
    title = "Trial Duration by Phase",
    x = "Phase",
    y = "Duration (months)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "none"
  )

ggsave("docs/02_duration_by_phase.png", p2, width = 8, height = 5, dpi = 150)
cat("📊 Saved: docs/02_duration_by_phase.png\n")

# Enrollment vs Duration
p3 <- ggplot(trials_clean, aes(x = enrollment_count, y = duration_months)) +
  geom_point(alpha = 0.3, color = "#0078D4") +
  geom_smooth(method = "lm", se = TRUE, color = "#D13438") +
  scale_x_log10(labels = comma) +
  labs(
    title = "Enrollment Size vs Trial Duration",
    subtitle = "Log scale for enrollment",
    x = "Enrollment Count (log scale)",
    y = "Duration (months)"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))

ggsave("docs/03_enrollment_vs_duration.png", p3, width = 8, height = 5, dpi = 150)
cat("📊 Saved: docs/03_enrollment_vs_duration.png\n")

# Sites vs Duration
p4 <- ggplot(trials_clean |> filter(n_sites <= 200), aes(x = n_sites, y = duration_months)) +
  geom_point(alpha = 0.3, color = "#0078D4") +
  geom_smooth(method = "lm", se = TRUE, color = "#D13438") +
  labs(
    title = "Number of Sites vs Trial Duration",
    subtitle = "Excluding trials with > 200 sites",
    x = "Number of Sites",
    y = "Duration (months)"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))

ggsave("docs/04_sites_vs_duration.png", p4, width = 8, height = 5, dpi = 150)
cat("📊 Saved: docs/04_sites_vs_duration.png\n")

# ----------------------------------------------------------------------------
# Final Summary
# ----------------------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat("✅ Data Exploration Complete!\n")
cat("============================================================\n\n")

cat("📁 Files Created:\n")
cat("   data-raw/cache/oncology_trials_raw.rds      (raw API response)\n")
cat("   data-raw/cache/oncology_trials_extracted.rds (all fields)\n")
cat("   data-raw/oncology_trials_clean.rds          (modeling-ready)\n")
cat("   data-raw/oncology_trials_clean.csv          (for inspection)\n")
cat("   docs/01_duration_distribution.png\n")
cat("   docs/02_duration_by_phase.png\n")
cat("   docs/03_enrollment_vs_duration.png\n")
cat("   docs/04_sites_vs_duration.png\n")

cat("\n📊 Dataset Summary:\n")
cat("   Total trials downloaded:", length(all_trials), "\n")
cat("   Trials ready for modeling:", nrow(trials_clean), "\n")
cat("   Features available:", ncol(trials_clean), "\n")

cat("\n🎯 Next Steps:\n")
cat("   1. Review visualizations in docs/ folder\n")
cat("   2. Examine trials_clean data frame for patterns\n")
cat("   3. Build 03_feature_engineering.R for model prep\n")
cat("   4. Start building predictive models\n")

cat("\n============================================================\n")
cat("   trials_clean object is available in your environment\n")
cat("============================================================\n")