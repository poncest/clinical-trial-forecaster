# =============================================================================
# Module: Duration Forecaster
# =============================================================================
# Purpose: Interactive prediction interface - users input trial characteristics
#          and receive predicted duration with context
# =============================================================================

mod_forecaster_ui <- function(id) {
  ns <- NS(id)
  
  div(
    # Header
    div(
      class = "tab-header",
      h2(class = "tab-title", icon("calculator"), " Duration Forecaster"),
      p(class = "tab-subtitle", 
        "Enter trial characteristics to predict expected duration")
    ),
    
    # Main layout: Input form + Results
    div(
      class = "ui two column stackable grid",
      
      # Left: Input Form
      div(
        class = "column",
        div(
          class = "ui segment",
          h3(style = paste0("color: ", colors$primary_dark, "; margin-top: 0;"), 
             "Trial Characteristics"),
          
          # Phase selection
          div(
            style = "margin-bottom: 1.5em;",
            tags$label(
              style = "font-weight: 600; display: block; margin-bottom: 0.5em;",
              icon("flask"), " Trial Phase"
            ),
            selectInput(
              ns("phase"),
              label = NULL,
              choices = c(
                "Phase 2" = "Phase 2",
                "Phase 1/2" = "Phase 1/2",
                "Phase 2/3" = "Phase 2/3",
                "Phase 3" = "Phase 3"
              ),
              selected = "Phase 3",
              width = "100%"
            )
          ),
          
          # Sponsor type
          div(
            style = "margin-bottom: 1.5em;",
            tags$label(
              style = "font-weight: 600; display: block; margin-bottom: 0.5em;",
              icon("building"), " Sponsor Type"
            ),
            selectInput(
              ns("sponsor"),
              label = NULL,
              choices = c(
                "Industry" = "Industry",
                "Academic/Other" = "Academic/Other",
                "NIH" = "NIH",
                "Government/Network" = "Government/Network"
              ),
              selected = "Industry",
              width = "100%"
            )
          ),
          
          # Target enrollment
          div(
            style = "margin-bottom: 1.5em;",
            tags$label(
              style = "font-weight: 600; display: block; margin-bottom: 0.5em;",
              icon("users"), " Target Enrollment"
            ),
            numericInput(
              ns("enrollment"),
              label = NULL,
              value = 200,
              min = 10,
              max = 5000,
              step = 10,
              width = "100%"
            ),
            div(
              style = paste0("font-size: 0.85em; color: ", colors$text_muted, "; margin-top: 0.3em;"),
              uiOutput(ns("enrollment_context"), inline = TRUE)
            )
          ),
          
          # Number of sites
          div(
            style = "margin-bottom: 1.5em;",
            tags$label(
              style = "font-weight: 600; display: block; margin-bottom: 0.5em;",
              icon("hospital"), " Number of Sites"
            ),
            numericInput(
              ns("sites"),
              label = NULL,
              value = 30,
              min = 1,
              max = 500,
              step = 1,
              width = "100%"
            ),
            div(
              style = paste0("font-size: 0.85em; color: ", colors$text_muted, "; margin-top: 0.3em;"),
              uiOutput(ns("sites_context"), inline = TRUE)
            )
          ),
          
          # Number of countries
          div(
            style = "margin-bottom: 1.5em;",
            tags$label(
              style = "font-weight: 600; display: block; margin-bottom: 0.5em;",
              icon("globe"), " Number of Countries"
            ),
            numericInput(
              ns("countries"),
              label = NULL,
              value = 5,
              min = 1,
              max = 50,
              step = 1,
              width = "100%"
            ),
            div(
              style = paste0("font-size: 0.85em; color: ", colors$text_muted, "; margin-top: 0.3em;"),
              uiOutput(ns("countries_context"), inline = TRUE)
            )
          ),
          
          # Registration to start (days)
          div(
            style = "margin-bottom: 1.5em;",
            tags$label(
              style = "font-weight: 600; display: block; margin-bottom: 0.5em;",
              icon("clock"), " Expected Startup Time (days)"
            ),
            numericInput(
              ns("reg_to_start"),
              label = NULL,
              value = 180,
              min = 0,
              max = 1000,
              step = 30,
              width = "100%"
            ),
            div(
              style = paste0("font-size: 0.85em; color: ", colors$text_muted, "; margin-top: 0.3em;"),
              "Days from registration to first patient enrolled"
            )
          ),
          
          # Predict button
          actionButton(
            ns("predict"),
            label = tagList(icon("play"), " Generate Prediction"),
            class = "ui primary button fluid",
            style = "margin-top: 1em;"
          )
        )
      ),
      
      # Right: Results
      div(
        class = "column",
        
        # Prediction Result Card
        div(
          class = "ui segment",
          style = "min-height: 200px;",
          uiOutput(ns("prediction_result"))
        ),
        
        # Context Card
        div(
          class = "ui segment",
          style = "margin-top: 1em;",
          h4(style = paste0("color: ", colors$primary_dark, "; margin-top: 0;"),
             icon("chart bar"), " How Does This Compare?"),
          uiOutput(ns("comparison_context"))
        ),
        
        # Derived metrics
        div(
          class = "ui segment",
          style = "margin-top: 1em;",
          h4(style = paste0("color: ", colors$primary_dark, "; margin-top: 0;"),
             icon("info circle"), " Trial Profile"),
          uiOutput(ns("trial_profile"))
        )
      )
    ),
    
    # Bottom: Interpretation guidance
    div(
      class = "ui segment",
      style = "margin-top: 1.5em;",
      
      h4(style = paste0("color: ", colors$primary_dark, "; margin-top: 0;"),
         icon("question circle"), " Interpreting Results"),
      
      div(
        class = "ui three column stackable grid",
        
        div(
          class = "column",
          tags$strong("What the prediction means"),
          p(style = "margin-top: 0.5em;",
            "The predicted duration represents the expected time from first patient enrolled 
             to last patient last visit, based on patterns in ", 
            format(app_info$n_trials, big.mark = ","), " historical oncology trials.")
        ),
        
        div(
          class = "column",
          tags$strong("Uncertainty range"),
          p(style = "margin-top: 0.5em;",
            "The model has median error of ", model_metrics$median_abs_error_months, 
            " months. Half of predictions fall within ±10 months of actual duration. 
             Treat predictions as estimates, not guarantees.")
        ),
        
        div(
          class = "column",
          tags$strong("Factors not captured"),
          p(style = "margin-top: 0.5em;",
            "Protocol complexity, patient population availability, competitive landscape, 
             and site performance vary significantly but aren't captured in registry data. 
             Use predictions as a starting point for planning.")
        )
      )
    )
  )
}

mod_forecaster_server <- function(id, trials_data, ref_stats, phase_levels, sponsor_levels) {
  moduleServer(id, function(input, output, session) {
    
    # Reactive: Store prediction
    prediction <- reactiveVal(NULL)
    
    # Context helpers
    output$enrollment_context <- renderUI({
      med <- round(ref_stats$enrollment_median, 0)
      diff <- input$enrollment - med
      if (abs(diff) < 20) {
        HTML(paste0("Near median (", med, " patients)"))
      } else if (diff > 0) {
        HTML(paste0("<span style='color:", colors$warning, ";'>", 
                    round(diff/med*100, 0), "% above median</span>"))
      } else {
        HTML(paste0(round(abs(diff)/med*100, 0), "% below median"))
      }
    })
    
    output$sites_context <- renderUI({
      med <- round(ref_stats$sites_median, 0)
      diff <- input$sites - med
      if (abs(diff) < 5) {
        HTML(paste0("Near median (", med, " sites)"))
      } else if (diff > 0) {
        HTML(paste0("<span style='color:", colors$warning, ";'>", 
                    round(diff/med*100, 0), "% above median</span>"))
      } else {
        HTML(paste0(round(abs(diff)/med*100, 0), "% below median"))
      }
    })
    
    output$countries_context <- renderUI({
      med <- round(ref_stats$countries_median, 0)
      if (input$countries == 1) {
        HTML("Single-country study")
      } else if (input$countries <= med) {
        HTML(paste0("Modest geographic spread (median: ", med, ")"))
      } else {
        HTML(paste0("<span style='color:", colors$warning, ";'>Global study (median: ", med, ")</span>"))
      }
    })
    
    # Generate prediction on button click
    observeEvent(input$predict, {
      
      # Validate inputs
      req(input$enrollment > 0, input$sites > 0, input$countries > 0)
      
      # Get prediction
      pred_months <- predict_duration(
        enrollment = input$enrollment,
        n_sites = input$sites,
        n_countries = input$countries,
        registration_to_start = input$reg_to_start,
        phase = input$phase,
        sponsor = input$sponsor
      )
      
      prediction(pred_months)
    })
    
    # Prediction result display
    output$prediction_result <- renderUI({
      
      if (is.null(prediction())) {
        # Initial state
        div(
          style = "text-align: center; padding: 3em; color: #666;",
          icon("calculator", style = "font-size: 3em; opacity: 0.3;"),
          p(style = "margin-top: 1em;", "Enter trial characteristics and click 'Generate Prediction'")
        )
      } else {
        # Show prediction
        pred <- prediction()
        formatted <- format_duration(pred)
        
        div(
          class = "prediction-result",
          div(class = "prediction-value", formatted),
          div(class = "prediction-label", "Predicted Trial Duration"),
          div(
            style = "margin-top: 1em; font-size: 0.9em; opacity: 0.85;",
            paste0("(", round(pred, 1), " months)")
          )
        )
      }
    })
    
    # Comparison context
    output$comparison_context <- renderUI({
      
      if (is.null(prediction())) {
        p(style = "color: #666;", "Prediction will be compared to historical data")
      } else {
        pred <- prediction()
        
        # Calculate percentile
        percentile <- mean(trials_data$duration_months <= pred, na.rm = TRUE) * 100
        
        # Compare to median
        vs_median <- ((pred / ref_stats$duration_median) - 1) * 100
        
        # Determine interpretation
        if (percentile < 25) {
          interpretation <- "shorter than most trials in our dataset"
          interp_color <- colors$success
        } else if (percentile < 50) {
          interpretation <- "below average duration"
          interp_color <- colors$success
        } else if (percentile < 75) {
          interpretation <- "above average duration"
          interp_color <- colors$warning
        } else {
          interpretation <- "longer than most trials in our dataset"
          interp_color <- colors$warning
        }
        
        div(
          # Percentile bar
          div(
            style = "margin-bottom: 1.5em;",
            div(
              style = "display: flex; justify-content: space-between; margin-bottom: 0.3em;",
              span("Shorter"),
              span("Longer")
            ),
            div(
              style = paste0("height: 8px; background: ", colors$border, "; border-radius: 4px; position: relative;"),
              div(
                style = paste0(
                  "position: absolute; left: ", percentile, "%; top: -4px; ",
                  "width: 16px; height: 16px; background: ", colors$primary, "; ",
                  "border-radius: 50%; transform: translateX(-50%); ",
                  "border: 2px solid white; box-shadow: 0 1px 3px rgba(0,0,0,0.2);"
                )
              )
            ),
            div(
              style = "text-align: center; margin-top: 0.5em; font-size: 0.9em;",
              paste0(round(percentile, 0), "th percentile")
            )
          ),
          
          # Stats
          div(
            class = "ui two column grid",
            div(
              class = "column",
              style = "text-align: center;",
              div(style = "font-size: 1.5em; font-weight: 600;",
                  paste0(ifelse(vs_median > 0, "+", ""), round(vs_median, 0), "%")),
              div(style = paste0("font-size: 0.85em; color: ", colors$text_muted), 
                  "vs. median duration")
            ),
            div(
              class = "column",
              style = "text-align: center;",
              div(style = "font-size: 1.5em; font-weight: 600;",
                  paste0(round(ref_stats$duration_median, 0), " mo")),
              div(style = paste0("font-size: 0.85em; color: ", colors$text_muted), 
                  "historical median")
            )
          ),
          
          # Interpretation
          div(
            style = paste0("margin-top: 1em; padding: 0.8em; background: ", colors$background, 
                           "; border-radius: 6px; border-left: 3px solid ", interp_color, ";"),
            p(style = "margin: 0;",
              "This prediction is ", tags$strong(interpretation), ".")
          )
        )
      }
    })
    
    # Trial profile (derived metrics)
    output$trial_profile <- renderUI({
      
      enrollment_per_site <- round(input$enrollment / input$sites, 1)
      is_multisite <- input$sites > 1
      is_multinational <- input$countries > 1
      
      tags$table(
        class = "ui very basic compact table",
        tags$tbody(
          tags$tr(
            tags$td("Enrollment per site"),
            tags$td(style = "text-align: right;", paste0(enrollment_per_site, " patients/site"))
          ),
          tags$tr(
            tags$td("Study type"),
            tags$td(style = "text-align: right;", 
                    ifelse(is_multisite, "Multi-site", "Single-site"))
          ),
          tags$tr(
            tags$td("Geographic scope"),
            tags$td(style = "text-align: right;", 
                    ifelse(is_multinational, "Multinational", "Single-country"))
          ),
          tags$tr(
            tags$td("Sites per country"),
            tags$td(style = "text-align: right;", 
                    paste0(round(input$sites / input$countries, 1), " avg"))
          )
        )
      )
    })
    
  })
}
