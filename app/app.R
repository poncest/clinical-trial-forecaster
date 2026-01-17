# =============================================================================
# Clinical Trial Duration Forecaster - Main App Entry Point
# =============================================================================
# Portfolio Project: Predictive Analytics Dashboard
# Author: Steven Ponce
# Date: January 2026
# =============================================================================

# Load shiny.semantic explicitly (failsafe)
library(shiny)
library(shiny.semantic)

# Source global setup (packages, data, helpers)
source("global.R")

# Source modules
source("modules/mod_executive_brief.R")
source("modules/mod_forecaster.R")
source("modules/mod_features.R")
source("modules/mod_performance.R")
source("modules/mod_methods.R")

# Source UI
source("R/ui_main.R")

# =============================================================================
# SERVER
# =============================================================================
server <- function(input, output, session) {
  
  # Initialize modules
  mod_executive_brief_server("exec_brief", trials_data, model_metrics, ref_stats)
  mod_forecaster_server("forecaster", trials_data, ref_stats, phase_levels, sponsor_levels)
  mod_features_server("features", trials_data, feature_importance)
  mod_performance_server("performance", test_predictions, model_comparison, model_metrics)
  mod_methods_server("methods", trials_data, app_info)
  
}

# =============================================================================
# RUN APP
# =============================================================================
shinyApp(ui = ui, server = server)