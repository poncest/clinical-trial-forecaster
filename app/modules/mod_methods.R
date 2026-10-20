# =============================================================================
# Module: Methods & Data
# =============================================================================
# Purpose: Document data sources, methodology, limitations, and disclaimers
# Stage 3: Time-based validation findings - R² dropped from 0.84 to 0.04
#          Added Validation Lessons panel and Why Forecasting Is Hard section
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
    
    # === VALIDATION LESSONS PANEL (Key Stage 3 Finding) ===
    div(
      class = "ui segment",
      style = "border-left: 4px solid #D32F2F; background: #FFF8F8;",
      
      h3(
        style = "color: #D32F2F; margin-top: 0;",
        icon("exclamation-triangle"),
        " Validation Lessons: Why Metrics Can Mislead"
      ),
      
      p(
        style = "font-size: 1.1em; line-height: 1.7; color: #1A1A2E;",
        "Random validation suggested strong performance (R² ≈ 0.84). ",
        "Time-based validation—appropriate for prospective forecasting—revealed ",
        "minimal predictive power (R² ≈ 0.04). This highlights both the limitations ",
        "of registry data and the importance of matching validation strategy to ",
        "decision context."
      ),
      
      # Comparison boxes
      div(
        class = "ui two column stackable grid",
        style = "margin-top: 1.5em;",
        
        # Random validation column
        div(
          class = "column",
          div(
            class = "ui segment",
            style = "background: #FFF3E0;",
            h4(style = "color: #E65100; margin-top: 0;", icon("random"), " Random Split Validation"),
            div(
              style = "font-size: 2em; font-weight: bold; color: #E65100;",
              "R² = 0.84"
            ),
            p(
              style = "color: #4A4A68; margin-top: 0.5em;",
              tags$strong("Problem:"), " Future data points mixed into training set. ",
              "Model learns patterns it wouldn't have access to in real forecasting."
            ),
            div(
              class = "ui label",
              style = "background: #E65100; color: white;",
              "Misleading for prospective use"
            )
          )
        ),
        
        # Time-based validation column
        div(
          class = "column",
          div(
            class = "ui segment",
            style = "background: #E8F5E9;",
            h4(style = "color: #2E7D32; margin-top: 0;", icon("calendar"), " Time-Based Validation"),
            div(
              style = "font-size: 2em; font-weight: bold; color: #2E7D32;",
              "R² = 0.04"
            ),
            p(
              style = "color: #4A4A68; margin-top: 0.5em;",
              tags$strong("Approach:"), " Train on ≤2015, test on 2016-2018. ",
              "Simulates real forecasting: predicting future from past only."
            ),
            div(
              class = "ui label",
              style = "background: #2E7D32; color: white;",
              "Honest estimate for decision-making"
            )
          )
        )
      ),
      
      # Key takeaway
      div(
        style = "margin-top: 1.5em; padding: 1em; background: #F5F5F5; border-radius: 4px;",
        p(
          style = "margin: 0; color: #1A1A2E;",
          tags$strong("Key Takeaway: "),
          "This model is ", tags$strong("not suitable for prospective duration forecasting "),
          "without additional data sources. The tool remains valuable for understanding ",
          "historical patterns and the factors associated with trial duration."
        )
      )
    ),
    
    # === WHY FORECASTING IS HARD SECTION ===
    div(
      class = "ui segment",
      style = "margin-top: 1.5em;",
      
      h3(
        style = paste0("color: ", colors$primary_dark, "; margin-top: 0;"),
        icon("question-circle"),
        " Why Trial Duration Forecasting Is Hard"
      ),
      
      h4(style = "color: #4A4A68;", "1. Trial Durations Are Changing Over Time"),
      p(
        "Our analysis revealed a ", tags$strong("~30-month reduction "),
        "in median trial duration between training (≤2015) and test (2016-2018) periods:"
      ),
      tags$ul(
        tags$li("Training period median: 81.9 months"),
        tags$li("Test period median: 52.7 months"),
        tags$li("This represents a fundamental shift in trial dynamics")
      ),
      
      p(style = "color: #4A4A68;", "Likely drivers of this acceleration in oncology:"),
      tags$ul(
        style = "color: #4A4A68;",
        tags$li("Adaptive trial designs"),
        tags$li("Biomarker-selected populations"),
        tags$li("Immuno-oncology platform efficiencies"),
        tags$li("Regulatory acceptance of surrogate endpoints"),
        tags$li("Improved site networks and startup tooling")
      ),
      
      h4(style = "color: #4A4A68; margin-top: 1.5em;", "2. Registry Data Lacks Key Predictive Signals"),
      p("ClinicalTrials.gov captures basic trial characteristics, but the true drivers ",
        "of trial duration are not consistently available:"),
      
      div(
        class = "ui two column stackable grid",
        div(
          class = "column",
          h5(style = "color: #2E7D32;", "Available in Registry"),
          tags$ul(
            tags$li("Phase"),
            tags$li("Enrollment target"),
            tags$li("Number of sites/countries"),
            tags$li("Sponsor type"),
            tags$li("Registration timing")
          )
        ),
        div(
          class = "column",
          h5(style = "color: #D32F2F;", "Missing (True Drivers)"),
          tags$ul(
            tags$li("Protocol complexity"),
            tags$li("Endpoint strategy"),
            tags$li("Competitive landscape"),
            tags$li("Country-specific startup constraints"),
            tags$li("Line-of-therapy and biomarker prevalence"),
            tags$li("Site experience and performance")
          )
        )
      ),
      
      div(
        style = "padding: 1em; background: #E3F2FD; border-radius: 4px; margin-top: 1em;",
        p(
          style = "margin: 0;",
          tags$strong("Appropriate uses of this tool: "),
          "Understanding historical duration distributions by phase, sponsor, and trial size; ",
          "exploring associations between trial characteristics and duration; ",
          "generating rough reference ranges (not point forecasts)."
        )
      )
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
            tags$li(tags$strong("Split: "), "Time-based (train ≤2015, test 2016-2018)"),
            tags$li(tags$strong("Training: "), "546 trials"),
            tags$li(tags$strong("Test: "), "340 trials"),
            tags$li(tags$strong("Excluded: "), "878 trials (>2018, right-truncated)"),
            tags$li(tags$strong("CV: "), "10-fold within training data"),
            tags$li(tags$strong("Selection: "), "Best CV R²")
          )
        ),
        
        div(
          class = "column",
          h4("Models Evaluated"),
          tags$ul(
            tags$li("Linear Regression (baseline)"),
            tags$li(tags$strong("Elastic Net (selected)")),
            tags$li("Random Forest (ensemble)"),
            tags$li("XGBoost (boosting)")
          ),
          # Stage 3: Updated findings
          p(style = paste0("font-size: 0.9em; color: ", colors$text_secondary),
            "Elastic Net selected for best CV performance. Time-based test set R² = 0.04, 
             indicating registry features have limited power for prospective forecasting. 
             See Validation Lessons above for details.")
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
        p("The registration-to-start lag input is only known ", tags$em("after"),
          " study startup. It was the strongest predictor in an earlier random-split model,
           but carries little weight in the final time-based model, where sponsor type
           dominates. For pre-startup forecasting, treat the startup lag input as an
           assumption to be validated.")
      ),
      
      div(
        class = "insight-box",
        style = paste0("border-left-color: ", colors$success, ";"),
        h4(style = "margin-top: 0;", "Validation Approach (Stage 3)"),
        p("This model uses ", tags$strong("time-based validation"), ": trained on trials 
           starting ≤2015, tested on trials starting 2016-2018. This approach simulates 
           real forecasting—predicting future trials from historical patterns only. 
           The resulting R² ≈ 0.04 reflects the honest prospective capability of registry-based 
           features, rather than the inflated R² ≈ 0.84 from random splits that allow 
           temporal leakage.")
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