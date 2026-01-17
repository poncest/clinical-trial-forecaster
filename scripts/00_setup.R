# ============================================================================
# 00_setup.R
# Clinical Trial Forecaster - Development Environment Setup
# ============================================================================
# Purpose: Install all required packages for development
# Usage:   Run ONCE when setting up the project (not at runtime)
# Author:  Steven Ponce
# Date:    January 2026
# ============================================================================

# This script is for DEVELOPMENT ONLY
# Do NOT deploy this file or run at app runtime
# See: shiny_deployment_lessons_learned.md

# ----------------------------------------------------------------------------
# Install pak if not already installed
# ----------------------------------------------------------------------------
if (!requireNamespace("pak", quietly = TRUE)) {
  
  install.packages("pak")
}

# ----------------------------------------------------------------------------
# Core Shiny & UI Framework
# ----------------------------------------------------------------------------
shiny_packages <- c(
  "shiny",
  "shiny.semantic",
  "semantic.dashboard",
  "bslib",
  "htmltools",
  "waiter"
)

# ----------------------------------------------------------------------------
# Data Manipulation
# ----------------------------------------------------------------------------
data_packages <- c(
  "dplyr",
  "tidyr",
  "purrr",
  "stringr",
  "lubridate",
  "forcats",
  "janitor",
  "glue"
)

# ----------------------------------------------------------------------------
# Data Visualization
# ----------------------------------------------------------------------------
viz_packages <- c(
  "ggplot2",
  "ggiraph",
  "reactable",
  "reactablefmtr",
  "scales",
  "ggtext",
  "patchwork"
)

# ----------------------------------------------------------------------------
# API & Data Acquisition
# ----------------------------------------------------------------------------
api_packages <- c(
  "httr2",
  "jsonlite",
  "ctrialsgov"
)

# ----------------------------------------------------------------------------
# Predictive Modeling (tidymodels ecosystem)
# ----------------------------------------------------------------------------
modeling_packages <- c(
  "tidymodels",
  "parsnip",
  "recipes",
  "rsample",
  "tune",
  "yardstick",
  "workflows",
  "broom",
  "probably"
)

# ----------------------------------------------------------------------------
# Model Types (engines for parsnip)
# ----------------------------------------------------------------------------
model_engine_packages <- c(
  "ranger",
  "xgboost",
  "glmnet",
  "survival"
)

# ----------------------------------------------------------------------------
# Utilities & Development
# ----------------------------------------------------------------------------
utility_packages <- c(
  "fs",
  "here",
  "config",
  "logger",
  "tictoc"
)

# ----------------------------------------------------------------------------
# Combine all packages
# ----------------------------------------------------------------------------
all_packages <- c(
  shiny_packages,
  data_packages,
  viz_packages,
  api_packages,
  modeling_packages,
  model_engine_packages,
  utility_packages
)

# ----------------------------------------------------------------------------
# Install all packages using pak
# ----------------------------------------------------------------------------
cat("============================================================\n")
cat("Installing packages for Clinical Trial Forecaster\n
")
cat("============================================================\n")
cat("Total packages:", length(all_packages), "\n\n")

pak::pak(all_packages)

cat("\n============================================================\n")
cat("Setup complete!\n")
cat("============================================================\n")

# ----------------------------------------------------------------------------
# Verify installation
# ----------------------------------------------------------------------------
cat("\nVerifying installations...\n\n")

installed <- sapply(all_packages, requireNamespace, quietly = TRUE)

if (all(installed)) {
  cat("All packages installed successfully.\n")
} else {
  missing <- all_packages[!installed]
  cat("WARNING: The following packages failed to install:\n")
  cat(paste(" -", missing, collapse = "\n"), "\n")
}

# ----------------------------------------------------------------------------
# Create Project Folder Structure
# ----------------------------------------------------------------------------
cat("\n============================================================\n")
cat("📁 Creating project folder structure...\n")
cat("============================================================\n\n")

# Define folder structure
dirs <- c(
  # App folders (deployed to shinyapps.io)
  "app/modules",
  "app/R",
  "app/data/processed",
  "app/www",
  
  # Development folders (NOT deployed)
  "scripts",
  "data-raw/cache",
  "models",
  "docs",
  "screenshots"
)

# Create folders
for (d in dirs) {
  fs::dir_create(d)
  cat(sprintf("  ✅ %s/\n", d))
}

cat("\n")
cat("Folder structure:\n")
cat("
clinical-trial-forecaster/
├── app/                    # Deployed to shinyapps.io
│   ├── modules/            # Shiny modules
│   ├── R/                  # Helper functions
│   ├── data/processed/     # Cleaned data for app
│   └── www/                # Static assets (CSS, images)
├── scripts/                # Development only
├── data-raw/cache/         # Raw API downloads
├── models/                 # Saved model objects
├── docs/                   # Documentation
└── screenshots/            # For README
")

# ----------------------------------------------------------------------------
# Session Info (for reproducibility)
# ----------------------------------------------------------------------------
cat("\n============================================================\n")
cat("Session Info\n")
cat("============================================================\n")
sessionInfo()