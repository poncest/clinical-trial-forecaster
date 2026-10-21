# =============================================================================
# Module: Methods & Data
# =============================================================================
# Purpose: Document data sources, methodology, limitations, and disclaimers
# Numbers come from model_metrics / sensitivity (global.R), which are computed
# from the saved model artifacts.
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
    
    # === CORRECTION NOTICE ===
    div(
      class = "ui segment",
      style = "border-left: 4px solid #F57C00; background: #FFF8E1;",
      h3(style = "color: #E65100; margin-top: 0;", icon("edit"), " Correction (October 2026)"),
      p(
        style = "line-height: 1.7; color: #1A1A2E; margin: 0;",
        "An independent review found that month-only registry dates (for example \"2005-08\") ",
        "had been read as dates in 2020. That mis-dated ", date_audit$misdated_in_both, " trials, ",
        "dropped ", format(date_audit$only_in_new_cohort, big.mark = ","), " others, ",
        "and produced a spurious random-split R² of 0.84 that earlier versions of this dashboard ",
        "presented as the headline result. A follow-up check found ", date_audit$duplicates_removed,
        " Phase 2/3 trials counted twice (returned by both phase queries). Dates are now parsed ",
        "correctly, duplicates removed, and every model retrained (counts: ",
        "scripts/07_date_parsing_audit.R). The validation lesson still holds, at a smaller and ",
        "honest size: R² of ",
        tags$strong(sprintf("%.2f", model_metrics$random_split_r_squared)), " on a random split versus ",
        tags$strong(sprintf("%.2f", model_metrics$r_squared)), " on later trials."
      )
    ),
    
    # === VALIDATION LESSONS PANEL ===
    div(
      class = "ui segment",
      style = "margin-top: 1.5em; border-left: 4px solid #D32F2F; background: #FFF8F8;",
      
      h3(
        style = "color: #D32F2F; margin-top: 0;",
        icon("exclamation-triangle"),
        " Validation Lessons: Same Model, Three Answers"
      ),
      
      local({
        drop_total <- model_metrics$random_split_r_squared - model_metrics$r_squared
        drop_window <- model_metrics$random_split_r_squared - model_metrics$random_trunc_r_squared
        share_window <- round(10 * drop_window / drop_total) * 10  # nearest 10%
        p(
          style = "font-size: 1.1em; line-height: 1.7; color: #1A1A2E;",
          "Holding the trials, features and model fixed, a random train/test split gives R² ≈ ",
          sprintf("%.2f", model_metrics$random_split_r_squared),
          ". Training on trials started by ", model_metrics$train_cutoff, " and testing on ",
          model_metrics$test_start, "–", model_metrics$test_end, " gives R² ≈ ",
          sprintf("%.2f", model_metrics$r_squared),
          ". Part of that drop is the test window, not the model: only 2016–2018 trials that had ",
          "finished by the data pull are included. Scoring the random split on trials that fit ",
          "the same window gives R² ≈ ", sprintf("%.2f", model_metrics$random_trunc_r_squared),
          ", so roughly ", share_window, "% of the drop comes from the window (",
          model_metrics$window_share_range[1], "–", model_metrics$window_share_range[2],
          "% across the five splits) and the rest from predicting later trials. That share is ",
          "probably a lower bound: the check uses one ", model_metrics$test_max_months,
          "-month cap, while trials started in ", model_metrics$test_end, " that finished ran at most ",
          model_metrics$test_last_year_max_months, " months. Either way, registry fields explain little of the variation."
        )
      }),
      
      # Comparison boxes
      div(
        class = "ui three column stackable grid",
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
              sprintf("R² = %.2f", model_metrics$random_split_r_squared)
            ),
            p(
              style = "color: #4A4A68; margin-top: 0.5em;",
              tags$strong("Problem:"), " test trials come from the same years as training trials, ",
              "so era-specific patterns are shared. It measures how well the model describes ",
              "history, not how well it forecasts new trials."
            ),
            div(
              class = "ui label",
              style = "background: #E65100; color: white;",
              "Optimistic for prospective use"
            )
          )
        ),

        # Random split, truncation-matched test window
        div(
          class = "column",
          div(
            class = "ui segment",
            style = "background: #FFF8E1;",
            h4(style = "color: #8D6E00; margin-top: 0;", icon("filter"), " Random Split, Same Window"),
            div(
              style = "font-size: 2em; font-weight: bold; color: #8D6E00;",
              sprintf("R² = %.2f", model_metrics$random_trunc_r_squared)
            ),
            p(
              style = "color: #4A4A68; margin-top: 0.5em;",
              tags$strong("Check:"), " the random split scored only on test trials no longer than ",
              model_metrics$test_max_months, " months, the longest time-based test trial. ",
              "Approximately separates the truncated test window from the effect of time."
            ),
            div(
              class = "ui label",
              style = "background: #8D6E00; color: white;",
              "Approximates the window effect"
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
              sprintf("R² = %.2f", model_metrics$r_squared)
            ),
            p(
              style = "color: #4A4A68; margin-top: 0.5em;",
              tags$strong("Approach:"), " train on ≤", model_metrics$train_cutoff, ", test on ",
              model_metrics$test_start, "–", model_metrics$test_end,
              ". Simulates real forecasting: predicting later trials from earlier ones only."
            ),
            div(
              class = "ui label",
              style = "background: #2E7D32; color: white;",
              "Closest to prospective use"
            )
          )
        )
      ),
      
      p(
        style = "margin-top: 1em; font-size: 0.85em; color: #4A4A68;",
        "R² here is 1 − SSE/SST on log duration (it can be negative when a model does worse than ",
        "predicting the average), so it also penalizes a shift in level: time-based test trials ",
        "ran on average about ", model_metrics$pct_shorter_than_predicted, "% shorter than predicted. ",
        "The squared correlation, which ignores level and measures linear association, is ",
        sprintf("%.2f", model_metrics$random_split_r_squared_cor), ", ",
        sprintf("%.2f", model_metrics$random_trunc_r_squared_cor), " and ",
        sprintf("%.2f", model_metrics$r_squared_cor), " for the three splits. Random-split figures ",
        "are the mean of 5 splits (scripts/06_random_vs_time_split.R)."
      ),
      
      # Key takeaway
      div(
        style = "margin-top: 1em; padding: 1em; background: #F5F5F5; border-radius: 4px;",
        p(
          style = "margin: 0; color: #1A1A2E;",
          tags$strong("Key Takeaway: "),
          "This model is ", tags$strong("not suitable for prospective duration forecasting "),
          "without additional data sources. It remains useful for understanding ",
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
      
      h4(style = "color: #4A4A68;", "1. Later Trials Look Shorter, Partly by Construction"),
      p(
        "Median duration is ", model_metrics$train_median_months, " months for trials started by ",
        model_metrics$train_cutoff, " and ", model_metrics$test_median_months, " months for trials started ",
        model_metrics$test_start, "–", model_metrics$test_end, ". Much of that gap is mechanical: ",
        "only completed trials are included, so a trial started in ", model_metrics$test_start, "–",
        model_metrics$test_end, " had to finish by the January 2026 data pull (longest: ",
        model_metrics$test_max_months, " months), while earlier trials could run up to 180 months. ",
        "Among earlier trials no longer than ", model_metrics$test_max_months,
        " months, the median is ", model_metrics$train_capped_median_months, " months."
      ),
      
      p(style = "color: #4A4A68;", "Possible contributors to any real change (hypotheses, not tested here):"),
      tags$ul(
        style = "color: #4A4A68;",
        tags$li("Adaptive trial designs"),
        tags$li("Biomarker-selected populations"),
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
            tags$li("Enrollment (final reported)"),
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
            tags$li("ACTUAL (final) enrollment between 5 and 10,000 participants"),
            tags$li("Duration between 1 and 180 months (outliers removed)")
          ),
          
          h4("Data Quality Notes"),
          p(style = paste0("font-size: 0.9em; color: ", colors$text_secondary),
            paste0("The January 2026 pull was capped at 2,000 trials per phase query (Phase 2 and ",
             "Phase 3, in the order the API returned them), so the cohort is a large sample of ",
             "completed oncology trials, not all of them. The return order shows no time trend ",
             "(correlation of position with start year ",
             paste(sprintf("%.2f", sampling_check$cor_position_start_year), collapse = " and "),
             "; median start year ", sampling_check$median_start_year_q1[1],
             " in the first and last quarter of each pull; trial IDs span the full range), so the cap ",
             "does not appear to cut by date (scripts/09_sampling_order_check.R). Month-only dates are ",
             "treated as the first of the month. Enrollment, sites and countries are final reported ",
             "values, not what was planned at study start."))
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
            tags$li(tags$strong("Training: "), format(model_metrics$train_n, big.mark = ","), " trials"),
            tags$li(tags$strong("Test: "), format(model_metrics$test_n, big.mark = ","), " trials"),
            tags$li(tags$strong("Excluded: "), format(temporal_split_info$excluded_recent_n, big.mark = ","),
                    " trials (started after ", model_metrics$test_end, "; only the shorter ones had finished)"),
            tags$li(tags$strong("CV: "), "10-fold within training data"),
            tags$li(tags$strong("Selection: "), "One-standard-error rule on CV RMSE; among tied models, Elastic Net by stated preference")
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
          p(style = paste0("font-size: 0.9em; color: ", colors$text_secondary),
            selection_note, " Fitted: penalty ", model_metrics$penalty,
            ", mixture ", model_metrics$mixture, ". Time-based test R² = ",
            sprintf("%.2f", model_metrics$r_squared), ". See Validation Lessons above.")
        )
      )
    ),
    
    # Feature Details Section
    div(
      class = "ui segment",
      style = "margin-top: 1.5em;",
      
      h3(style = paste0("color: ", colors$primary_dark, "; margin-top: 0;"),
         icon("list"), " Feature Definitions"),
      
      reactableOutput(ns("feature_definitions")),
      p(style = paste0("font-size: 0.85em; color: ", colors$text_secondary, "; margin-top: 0.75em;"),
        "All numeric and binary features are standardized (z-scores) before fitting, so their 
         coefficients are per standard deviation. Phase and sponsor are dummy-coded against 
         Phase 2 and Industry, so their coefficients compare each level with that reference.")
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
        h4(style = "margin-top: 0;", "Inputs Are Known Only in Hindsight"),
        p(paste0("Enrollment, number of sites and number of countries are the final values reported ",
           "at completion, not what was planned. Registration-to-start lag is the gap between ",
           "first registration and study start; it is negative for the many trials registered ",
           "after they started. Refitting with only what is known at study start (phase, sponsor, ",
           "registration-to-start lag) drops R² from ",
           sprintf("%.2f", prestart_r2("all (published model)", "random")), " to ",
           sprintf("%.2f", prestart_r2("pre-start only", "random")), " on a random split and from ",
           sprintf("%.2f", prestart_r2("all (published model)", "time-based")), " to ",
           sprintf("%.2f", prestart_r2("pre-start only", "time-based")),
           " on later trials (worse than predicting the average). Most of the signal comes from ",
           "inputs known only in hindsight (scripts/08_prestart_features.R)."))
      ),
      
      div(
        class = "insight-box",
        style = paste0("border-left-color: ", colors$success, ";"),
        h4(style = "margin-top: 0;", "Validation Approach"),
        p("This model uses ", tags$strong("time-based validation"), ": trained on trials 
           starting ≤", model_metrics$train_cutoff, ", tested on trials starting ",
          model_metrics$test_start, "–", model_metrics$test_end, ". The test window is 
           right-truncated: only trials finished by January 2026 are included, so long 2016–2018 
           trials are missing. That makes errors in months look smaller than they would be 
           for a complete cohort and shifts test durations down, which lowers R². The 
           truncation-matched random split in Validation Lessons separates the two effects.")
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
      ),

      div(
        class = "insight-box",
        h4(style = "margin-top: 0;", "Cohort Definition (Sensitivity-Checked)"),
        if (!is.null(sensitivity)) {
          p(paste0("The oncology cohort comes from a keyword-based registry query, so some trials are not oncology (e.g., uterine fibroids, rheumatoid arthritis); the crude check also flags some true oncology trials (e.g., 'Recurrent GBM'). About ", round(100 * sensitivity$n_no_oncology_term[1] / sensitivity$n_modeled[1]), "% of modeled trials (", sensitivity$n_no_oncology_term[1], " of ", format(sensitivity$n_modeled[1], big.mark = ","), ") list no cancer term. Refitting without them changes test R² from ", sprintf("%.3f", sensitivity$test_r2_trad[1]), " to ", sprintf("%.3f", sensitivity$test_r2_trad[2]), " and median absolute error from ", sensitivity$median_abs_err_mo[1], " to ", sensitivity$median_abs_err_mo[2], " months, with the same top predictors. Conclusions are unchanged. See scripts/05_sensitivity_oncology_filter.R."))
        }
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
          "Natural log of (enrollment / (sites + 1) + 1)",
          "Number of countries with participating sites",
          "Days from first ClinicalTrials.gov registration to study start (negative if registered after start)",
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
          "Numeric (integer, days)",
          "Categorical (reference: Phase 2)",
          "Categorical (reference: Industry)",
          "Binary 0/1 (standardized)",
          "Binary 0/1 (standardized)"
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