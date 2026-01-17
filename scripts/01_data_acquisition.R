# ============================================================================
# 01_data_acquisition.R
# Clinical Trial Forecaster - Data Acquisition from ClinicalTrials.gov
# ============================================================================
# Purpose: Download and explore clinical trial data from ClinicalTrials.gov
# Author:  Steven Ponce
# Date:    January 2026
# ============================================================================

# ----------------------------------------------------------------------------
# Setup
# ----------------------------------------------------------------------------
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(janitor)
library(httr2)
library(jsonlite)
library(fs)
library(tictoc)

# Create output directory
dir_create("data-raw")

# Define null coalescing operator (if not available from rlang)
`%||%` <- function(x, y) if (is.null(x)) y else x

# ----------------------------------------------------------------------------
# Option 1: Using ctrialsgov package (recommended to try first)
# ----------------------------------------------------------------------------

# The ctrialsgov package provides a convenient interface to ClinicalTrials.gov
# Documentation: https://cran.r-project.org/package=ctrialsgov

# Uncomment and run to explore:
# library(ctrialsgov)
# 
# # Example: Search for oncology trials
# oncology_trials <- ctgov_query(
#   study_type = "Interventional",
#   condition_terms = "cancer",
#   phase = c("Phase 2", "Phase 3"),
#   status = c("Completed", "Active, not recruiting")
# )

# ----------------------------------------------------------------------------
# Option 2: Direct API access using httr2
# ----------------------------------------------------------------------------

# ClinicalTrials.gov API documentation:
# https://clinicaltrials.gov/data-api/api

# Base URL for the new API (v2)
base_url <- "https://clinicaltrials.gov/api/v2/studies"

# Example: Query oncology trials
# This is a starting point - we'll refine based on exploration

query_clinicaltrials <- function(
    condition = "cancer",
    page_size = 10,
    page_token = NULL
) {
  
  # Build request - simplified query first
  
  req <- request(base_url) |>
    req_url_query(
      `query.cond` = condition,
      pageSize = page_size
    )
  
  # Add page token if paginating
  if (!is.null(page_token)) {
    req <- req |> req_url_query(pageToken = page_token)
  }
  
  
  # Add timeout for slow connections
  req <- req |> req_timeout(60)
  
  # Perform request
  resp <- req |>
    req_perform()
  
  # Parse response
  resp |>
    resp_body_json()
}

# ----------------------------------------------------------------------------
# Exploration: Understand the data structure
# ----------------------------------------------------------------------------

cat("\n============================================================\n")
cat("🔍 Running sample API query...\n")
cat("============================================================\n\n")

cat("Query: condition = 'cancer', page_size = 10\n\n")

tic("API Query")
sample_data <- tryCatch({
  query_clinicaltrials(
    condition = "cancer",
    page_size = 10
  )
}, error = function(e) {
  cat("❌ API Error:", conditionMessage(e), "\n")
  cat("\nTroubleshooting:\n")
  cat("1. Check internet connection\n")
  cat("2. Try: https://clinicaltrials.gov/api/v2/studies?pageSize=5 in browser\n")
  cat("3. API may be temporarily down\n")
  return(NULL)
})
toc()

# Exit early if API failed
if (is.null(sample_data)) {
  cat("\n⚠️  Script stopped due to API error. Fix connection and re-run.\n")
  stop("API query failed")
}

# ----------------------------------------------------------------------------
# Examine the response structure
# ----------------------------------------------------------------------------

cat("\n============================================================\n")
cat("📊 Response Structure\n")
cat("============================================================\n\n")

cat("Top-level elements:\n")
print(names(sample_data))

cat("\nNumber of trials returned:", length(sample_data$studies), "\n")

cat("\nFirst trial - available sections:\n")
print(names(sample_data$studies[[1]]))

cat("\nProtocol section fields:\n")
print(names(sample_data$studies[[1]]$protocolSection))

# ----------------------------------------------------------------------------
# Extract key fields from first trial (example)
# ----------------------------------------------------------------------------

cat("\n============================================================\n")
cat("📋 Sample Trial Details (First Record)\n")
cat("============================================================\n\n")

first_trial <- sample_data$studies[[1]]$protocolSection

# Identification
cat("NCT ID:", first_trial$identificationModule$nctId, "\n")
cat("Title:", substr(first_trial$identificationModule$briefTitle, 1, 80), "...\n")

# Status
cat("\nStatus:", first_trial$statusModule$overallStatus, "\n")
cat("Start Date:", first_trial$statusModule$startDateStruct$date %||% "NA", "\n")
cat("Completion Date:", first_trial$statusModule$completionDateStruct$date %||% "NA", "\n")

# Design
cat("\nPhase:", paste(first_trial$designModule$phases, collapse = ", "), "\n")
cat("Enrollment:", first_trial$designModule$enrollmentInfo$count %||% "NA", "\n")
cat("Enrollment Type:", first_trial$designModule$enrollmentInfo$type %||% "NA", "\n")

# Conditions
cat("\nConditions:", paste(first_trial$conditionsModule$conditions[1:min(3, length(first_trial$conditionsModule$conditions))], collapse = ", "), "\n")

# ----------------------------------------------------------------------------
# Key fields we'll need for forecasting
# ----------------------------------------------------------------------------

# Based on ClinicalTrials.gov data dictionary, relevant fields include:
#
# IDENTIFICATION:
# - nctId: Unique trial identifier
# - briefTitle: Trial name
# - officialTitle: Full title
#
# STATUS:
# - overallStatus: Current status
# - startDate: Study start
# - completionDate: Primary completion
# - studyFirstSubmitDate: Registration date
#
# DESIGN:
# - studyType: Interventional vs Observational
# - phases: Phase 1, 2, 3, 4
# - enrollmentInfo: Target enrollment, actual enrollment
#
# CONDITIONS:
# - conditions: Disease/condition studied
#
# LOCATIONS:
# - locations: Countries, facilities
#
# SPONSORS:
# - leadSponsor: Primary sponsor
# - collaborators: Other organizations

# ----------------------------------------------------------------------------
# Next Steps (to be developed)
# ----------------------------------------------------------------------------

# 1. Build robust pagination to download all oncology trials
# 2. Parse nested JSON into flat data frame
# 3. Handle missing data and inconsistencies
# 4. Filter to trials with complete enrollment data
# 5. Save processed data to data-raw/

# ----------------------------------------------------------------------------
# Save sample data for offline exploration
# ----------------------------------------------------------------------------

saveRDS(sample_data, "data-raw/sample_api_response.rds")
cat("\n✅ Sample data saved to: data-raw/sample_api_response.rds\n")

# ----------------------------------------------------------------------------
# Key fields we need for forecasting
# ----------------------------------------------------------------------------

cat("\n============================================================\n")
cat("📝 Key Fields for Forecasting Model\n")
cat("============================================================\n")

cat("
IDENTIFICATION:
- nctId: Unique trial identifier
- briefTitle: Trial name

STATUS & DATES:
- overallStatus: Current status (Completed, Active, etc.)
- startDateStruct$date: Study start
- completionDateStruct$date: Primary completion  
- studyFirstSubmitDate: Registration date

DESIGN:
- phases: Phase 1, 2, 3, 4
- enrollmentInfo$count: Target/actual enrollment
- enrollmentInfo$type: ESTIMATED vs ACTUAL

CONDITIONS:
- conditions: Disease/condition studied

LOCATIONS:
- Will need to explore contactsLocationsModule

SPONSORS:
- leadSponsor: Primary sponsor type (Industry, NIH, etc.)
")

# ----------------------------------------------------------------------------
# Next Steps
# ----------------------------------------------------------------------------

cat("\n============================================================\n")
cat("🎯 Next Steps\n")
cat("============================================================\n")

cat("
1. EXPLORE: Run in console to dig deeper:
   - str(sample_data$studies[[1]]$protocolSection, max.level = 3)
   - View locations: sample_data$studies[[1]]$protocolSection$contactsLocationsModule

2. ASSESS DATA QUALITY: Check which fields are consistently populated
   - How many trials have actual (not estimated) enrollment?
   - How many have completion dates?

3. SCALE UP: Modify query to pull more data:
   - Increase page_size to 100
   - Add pagination to get all oncology trials
   - Filter to trials with ACTUAL enrollment data

4. DOCUMENT: Create docs/data_exploration_notes.md with findings

5. NEXT SCRIPT: Build 02_data_exploration.R for deeper EDA
")

cat("\n============================================================\n")
cat("✅ 01_data_acquisition.R complete\n
")
cat("   sample_data object is available in your environment\n")
cat("============================================================\n")