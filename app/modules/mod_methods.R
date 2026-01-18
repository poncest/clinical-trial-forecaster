# =============================================================================
# Module: Methods & Data
# =============================================================================
# Purpose: Document data sources, methodology, limitations, and disclaimers
# Stage 2.5: Updated R² metric and fixed causal language on feature importance
# =============================================================================

mod_methods_ui <- function(id) {
  ns <- NS(id)
  
  div(
    # Header
    div(
      class = "tab-header",
      h2(class = "tab-title", icon("book"), " Methods & Data"),
      p(class = "tab-subtitle", 
        "Data sources, model methodology, and important limitations")
    ),
    
    # Data Source Section
    div(
      class = "ui segment",
      
      h3(style = paste0("color: ", colors$primary_dark, "; margin-top: 0;"),
         icon("database"), " Data Source"),
      
      div(
        class = "ui two column stackable grid",
        
        div(
          class = "column",
          h4("ClinicalTrials.gov"),
          p("This model was trained on publicly available clinical trial data from ",
            tags$a(href = "https://clinicaltrials.gov", target = "_blank", "ClinicalTrials.gov"),
            ", the U.S. National Library of Medicine's registry of clinical studies."),
          
          tags$table(
            class = "ui very basic compact table",
            tags$tbody(
              tags$tr(
                tags$td(tags$strong("Therapeutic Area")),
                tags$td(app_info$therapeutic_area)
              ),
              tags$tr(
                tags$td(tags$strong("Trial Phases")),
                tags$td(app_info$phases)
              ),
              tags$tr(
                tags$td(tags$strong("Trial Status")),
                tags$td("Completed only")
              ),
              tags$tr(
                tags$td(tags$strong("Total Trials")),
                tags$td(format(app_info$n_trials, big.mark = ","))
              ),
              tags$tr(
                tags$td(tags$strong("Data Accessed")),
                tags$td("January 2026")
              )
            )
          )
        ),
        
        div(
          class = "column",
          h4("Inclusion Criteria"),
          tags$ul(
            tags$li(tags$strong("Interventional trials only"), " (observational studies excluded)"),
            tags$li(tags$strong("Treatment purpose only"), " (excludes prevention, supportive care, screening)"),
            tags$li("Condition contains 'cancer' (broad oncology filter)"),
            tags$li("Phase 2, Phase 1/2, Phase 2/3, or Phase 3"),
            tags$li("Completed status with valid start and completion dates"),
            tags$li("ACTUAL (final) enrollment data (> 0 participants)"),
            tags$li("Duration between 1 and 180 months (outliers removed)")
          ),
          
          h4("Data Quality Notes"),
          p(style = paste0("font-size: 0.9em; color: ", colors$text_secondary),
            "Registry enrollment values reflect final reported enrollment ('ACTUAL'), not 
             anticipated enrollment at study start. Predictions assume inputs reflect 
             values consistent with completed trial characteristics.")
        )
      )
    ),
    
    # Methodology Section
    div(
      class = "ui segment",
      style = "margin-top: 1.5em;",
      
      h3(style = paste0("color: ", colors$primary_dark, "; margin-top: 0;"),
         icon("cogs"), " Modeling Methodology"),
      
      div(
        class = "ui three column stackable grid",
        
        div(
          class = "column",
          h4("Feature Engineering"),
          tags$ul(
            tags$li(tags$strong("Target: "), "Log-transformed duration (start date to completion date, in months)"),
            tags$li(tags$strong("Log transforms: "), "Enrollment, sites, enrollment/site"),
            tags$li(tags$strong("Categorical encoding: "), "Phase, sponsor type"),
            tags$li(tags$strong("Derived features: "), "Multi-site flag, multinational flag"),
            tags$li(tags$strong("Scaling: "), "Numeric features normalized")
          ),
          p(style = paste0("font-size: 0.85em; color: ", colors$text_secondary, "; font-style: italic;"),
            "Note: Duration is measured from study start to completion date as recorded in 
             ClinicalTrials.gov, which includes follow-up periods. This differs from 
             'last patient last visit' operational milestones.")
        ),
        
        div(
          class = "column",
          h4("Model Training"),
          tags$ul(
            tags$li(tags$strong("Split: "), "80% train / 20% test"),
            tags$li(tags$strong("Stratification: "), "By trial phase"),
            tags$li(tags$strong("Validation: "), "10-fold cross-validation"),
            tags$li(tags$strong("Tuning: "), "Grid search over hyperparameters"),
            tags$li(tags$strong("Selection: "), "Lowest CV RMSE")
          )
        ),
        
        div(
          class = "column",
          h4("Models Evaluated"),
          tags$ul(
            tags$li("Linear Regression (baseline)"),
            tags$li("Elastic Net (regularized linear)"),
            tags$li("Random Forest (ensemble)"),
            tags$li(tags$strong("XGBoost (selected)"))
          ),
          # Stage 2.5: Fixed causal language - "accounts for" → appropriate hedging
          p(style = paste0("font-size: 0.9em; color: ", colors$text_secondary),
            "XGBoost was selected for best CV performance (R² = 0.84). 
             Registration-to-start is the strongest signal in this dataset and 
             dominates importance metrics; it may reflect a mix of startup 
             complexity and registry update patterns.")
        )
      )
    ),
    
    # Feature Details Section
    div(
      class = "ui segment",
      style = "margin-top: 1.5em;",
      
      h3(style = paste0("color: ", colors$primary_dark, "; margin-top: 0;"),
         icon("list"), " Feature Definitions"),
      
      reactableOutput(ns("feature_definitions"))
    ),
    
    # What This Does NOT Do Section
    div(
      class = "ui segment",
      style = paste0("margin-top: 1.5em; border-left: 4px solid ", colors$warning, ";"),
      
      h3(style = paste0("color: ", colors$primary_dark, "; margin-top: 0;"),
         icon("exclamation triangle"), " What This Model Does NOT Do"),
      
      div(
        class = "ui two column stackable grid",
        
        div(
          class = "column",
          tags$ul(
            tags$li(
              tags$strong("Does not predict individual trial success"),
              " – The model predicts duration, not probability of approval or clinical outcomes."
            ),
            tags$li(
              tags$strong("Does not account for protocol-specific complexity"),
              " – Registry data lacks information about endpoints, biomarker requirements, 
                or adaptive designs."
            ),
            tags$li(
              tags$strong("Does not capture site performance variation"),
              " – All sites are treated equally; actual performance varies significantly."
            )
          )
        ),
        
        div(
          class = "column",
          tags$ul(
            tags$li(
              tags$strong("Does not model enrollment curves"),
              " – Duration is predicted as a single point estimate, not an enrollment trajectory."
            ),
            tags$li(
              tags$strong("Does not incorporate therapeutic area nuances"),
              " – Oncology-only training means predictions may not transfer to other areas."
            ),
            tags$li(
              tags$strong("Does not provide financial projections"),
              " – Duration estimates must be combined with separate cost models for budgeting."
            )
          )
        )
      )
    ),
    
    # Limitations Section
    div(
      class = "ui segment",
      style = "margin-top: 1.5em;",
      
      h3(style = paste0("color: ", colors$primary_dark, "; margin-top: 0;"),
         icon("info circle"), " Known Limitations"),
      
      div(
        class = "insight-box",
        style = paste0("border-left-color: ", colors$warning, ";"),
        h4(style = "margin-top: 0;", "Prediction Timepoint Assumption"),
        p("The top predictor (registration-to-start lag) is only known ", tags$em("after"), 
          " study startup. This model is best suited for predictions made at or after study 
           start, not for early planning when startup time must itself be estimated. For 
           pre-startup forecasting, treat the startup lag input as an assumption to be 
           validated.")
      ),
      
      div(
        class = "insight-box",
        style = paste0("border-left-color: ", colors$warning, ";"),
        h4(style = "margin-top: 0;", "Validation Approach"),
        p("The model was validated using random train/test splits stratified by phase, not 
           time-based splits. Performance metrics may overestimate accuracy for forecasting 
           future trials, as clinical trial operations evolve over time. A time-based 
           validation (train on earlier years, test on later years) would provide more 
           conservative and realistic performance estimates.")
      ),
      
      div(
        class = "insight-box",
        h4(style = "margin-top: 0;", "Survivorship Bias"),
        p("The model is trained only on completed trials. Terminated, suspended, or 
           withdrawn trials are excluded, potentially biasing estimates toward 
           'successful' trial characteristics.")
      ),
      
      div(
        class = "insight-box",
        h4(style = "margin-top: 0;", "Historical Patterns May Not Persist"),
        p("Clinical trial operations continue to evolve. Decentralized trials, 
           real-world data integration, and regulatory changes may alter duration 
           patterns not reflected in historical data.")
      ),
      
      div(
        class = "insight-box",
        h4(style = "margin-top: 0;", "Registry Data Quality"),
        p("ClinicalTrials.gov data is self-reported by sponsors. Data quality varies, 
           and some fields (particularly site counts and enrollment figures) may be 
           updated inconsistently throughout the trial lifecycle. Enrollment values 
           in the training data reflect 'actual' (final) enrollment, not 'anticipated' 
           enrollment available at planning time.")
      )
    ),
    
    # Technology Stack
    div(
      class = "ui segment",
      style = "margin-top: 1.5em;",
      
      h3(style = paste0("color: ", colors$primary_dark, "; margin-top: 0;"),
         icon("code"), " Technology Stack"),
      
      div(
        class = "ui five column stackable grid",
        style = "text-align: center;",
        
        div(
          class = "column",
          div(style = "font-size: 2em; margin-bottom: 0.3em;", icon("r project")),
          div(style = "font-weight: 600;", "R"),
          div(style = paste0("font-size: 0.85em; color: ", colors$text_muted), "Core language")
        ),
        
        div(
          class = "column",
          div(style = "font-size: 2em; margin-bottom: 0.3em;", icon("chart area")),
          div(style = "font-weight: 600;", "tidymodels"),
          div(style = paste0("font-size: 0.85em; color: ", colors$text_muted), "ML framework")
        ),
        
        div(
          class = "column",
          div(style = "font-size: 2em; margin-bottom: 0.3em;", icon("window maximize")),
          div(style = "font-weight: 600;", "Shiny"),
          div(style = paste0("font-size: 0.85em; color: ", colors$text_muted), "Web framework")
        ),
        
        div(
          class = "column",
          div(style = "font-size: 2em; margin-bottom: 0.3em;", icon("paint brush")),
          div(style = "font-weight: 600;", "shiny.semantic"),
          div(style = paste0("font-size: 0.85em; color: ", colors$text_muted), "UI components")
        ),
        
        div(
          class = "column",
          div(style = "font-size: 2em; margin-bottom: 0.3em;", icon("mouse pointer")),
          div(style = "font-weight: 600;", "ggiraph"),
          div(style = paste0("font-size: 0.85em; color: ", colors$text_muted), "Interactive plots")
        )
      )
    ),
    
    # References
    div(
      class = "ui segment",
      style = "margin-top: 1.5em;",
      
      h3(style = paste0("color: ", colors$primary_dark, "; margin-top: 0;"),
         icon("bookmark"), " References"),
      
      tags$ul(
        tags$li(
          "ClinicalTrials.gov. U.S. National Library of Medicine. ",
          tags$a(href = "https://clinicaltrials.gov", target = "_blank", "https://clinicaltrials.gov")
        ),
        tags$li(
          "Kuhn M, Wickham H (2020). tidymodels: Easily Install and Load the 'Tidymodels' Packages. ",
          tags$a(href = "https://tidymodels.org", target = "_blank", "https://tidymodels.org")
        ),
        tags$li(
          "Chang W, et al. (2023). shiny: Web Application Framework for R. ",
          tags$a(href = "https://shiny.posit.co", target = "_blank", "https://shiny.posit.co")
        ),
        tags$li(
          "Appsilon (2023). shiny.semantic: Semantic UI Support for Shiny. ",
          tags$a(href = "https://appsilon.github.io/shiny.semantic/", target = "_blank", 
                 "https://appsilon.github.io/shiny.semantic/")
        )
      )
    )
  )
}

mod_methods_server <- function(id, trials_data, app_info) {
  moduleServer(id, function(input, output, session) {
    
    # Feature definitions table
    output$feature_definitions <- renderReactable({
      
      feature_defs <- data.frame(
        Feature = c(
          "log_enrollment",
          "log_sites",
          "log_enrollment_per_site",
          "n_countries",
          "registration_to_start",
          "phase_simple",
          "sponsor_simple",
          "is_multisite",
          "is_multinational"
        ),
        Definition = c(
          "Natural log of enrollment (actual/final reported value)",
          "Natural log of (number of sites + 1)",
          "Natural log of (enrollment / sites + 1)",
          "Number of countries with participating sites",
          "Days from ClinicalTrials.gov registration to study start (known only at/after startup)",
          "Simplified phase category (Phase 2, 1/2, 2/3, 3)",
          "Sponsor type (Industry, Academic/Other, NIH, Gov/Network)",
          "Binary: trial has more than one site",
          "Binary: trial conducted in more than one country"
        ),
        `Data Type` = c(
          "Numeric (continuous)",
          "Numeric (continuous)",
          "Numeric (continuous)",
          "Numeric (integer)",
          "Numeric (integer, days) ⚠️",
          "Categorical (4 levels)",
          "Categorical (4 levels)",
          "Binary (0/1)",
          "Binary (0/1)"
        ),
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
      
      reactable(
        feature_defs,
        columns = list(
          Feature = colDef(
            width = 180,
            style = list(fontFamily = "monospace", fontWeight = 500)
          ),
          Definition = colDef(minWidth = 250),
          `Data Type` = colDef(width = 150)
        ),
        bordered = FALSE,
        striped = TRUE,
        highlight = TRUE,
        style = list(fontSize = "0.9em")
      )
    })
    
  })
}