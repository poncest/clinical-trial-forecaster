# =============================================================================
# Module: Duration Forecaster
# =============================================================================
# Validation caveat banner uses the computed time-based R² (model_metrics)
# Uses the saved Elastic Net workflow via predict(final_workflow, new_data)
# Option A architecture: App passes raw inputs, recipe handles transforms
# =============================================================================

mod_forecaster_ui <- function(id) {
  ns <- NS(id)
  
  div(
    div(
      class = "tab-header",
      h2(class = "tab-title", icon("calculator"), " Duration Forecaster"),
      p(class = "tab-subtitle", "Enter trial characteristics to predict expected duration")
    ),
    
    # Stage 3: Validation caveat warning
    div(
      class = "ui warning message",
      style = "margin-bottom: 1.5em;",
      div(class = "header", 
          icon("exclamation-triangle"), " Forecasting Limitation"),
      p(
        sprintf("Time-based validation indicates limited prospective accuracy (R² ≈ %.2f on trials started %s–%s). ",
                model_metrics$r_squared, model_metrics$test_start, model_metrics$test_end),
        "Predictions reflect historical patterns that may not apply to future trials. ",
        "Use for reference ranges only, not operational planning. ",
        tags$a(
          href = "#methods",
          onclick = "$('[data-tab=\"methods\"]').click(); return false;",
          style = "font-weight: 600;",
          "See Methods tab for details."
        )
      )
    ),
    
    div(
      class = "ui two column stackable grid",
      
      # Left: Inputs
      div(
        class = "column",
        div(
          class = "ui segment",
          h3(style = paste0("color: ", colors$primary_dark, "; margin-top: 0;"), "Trial Characteristics"),
          
          p(style = paste0("color: ", colors$text_muted, "; font-size: 0.9em; margin-bottom: 1.5em;"),
            "Configure parameters and click ", tags$strong("Generate Prediction"), "."),
          
          selectInput(ns("phase"), "Trial Phase",
                      choices = c("Phase 2", "Phase 1/2", "Phase 2/3", "Phase 3"), selected = "Phase 3"),
          
          selectInput(ns("sponsor"), "Sponsor Type",
                      choices = c("Industry", "Academic/Other", "NIH", "Government/Network"), selected = "Industry"),
          
          numericInput(ns("enrollment"), "Enrollment", value = 200, min = 5, max = 10000),
          p(style = paste0("font-size: 0.8em; color: ", colors$text_muted, "; margin-top: 5px; margin-bottom: 15px;"),
            "Model trained on actual (final) enrollment values"),
          
          numericInput(ns("sites"), "Number of Sites", value = 30, min = 1, max = 1000),
          numericInput(ns("countries"), "Number of Countries", value = 5, min = 1, max = 45),
          numericInput(ns("reg_to_start"), "Registration-to-Start Lag (days)", value = 0, min = -1825, max = 1500),
          p(style = paste0("font-size: 0.8em; color: ", colors$text_muted, "; margin-top: 5px; margin-bottom: 15px;"),
            tags$em(paste0("Days from first registration to study start; negative if registered after start ",
                           "(median ", round(ref_stats$reg_to_start_median), " days). Carries little weight in this model."))),
          uiOutput(ns("input_errors")),
          
          actionButton(ns("predict"), 
                       label = tagList(icon("play"), " Generate Prediction"),
                       class = "ui primary button fluid")
        )
      ),
      
      # Right: Results
      div(
        class = "column",
        
        # Main prediction result
        div(
          class = "ui segment",
          style = "min-height: 180px;",
          uiOutput(ns("prediction_result"))
        ),
        
        # How Does This Compare
        div(
          class = "ui segment",
          style = "margin-top: 1em;",
          h4(style = paste0("color: ", colors$primary_dark, "; margin-top: 0;"),
             icon("chart bar"), " How Does This Compare?"),
          uiOutput(ns("comparison"))
        ),
        
        # Trial Profile
        div(
          class = "ui segment",
          style = "margin-top: 1em;",
          h4(style = paste0("color: ", colors$primary_dark, "; margin-top: 0;"),
             icon("info circle"), " Trial Profile"),
          uiOutput(ns("profile"))
        )
      )
    ),
    
    # Bottom: Interpreting Results
    div(
      class = "ui segment",
      style = "margin-top: 1.5em;",
      h4(style = paste0("color: ", colors$primary_dark, "; margin-top: 0;"),
         icon("question circle"), " Interpreting Results"),
      div(
        class = "ui three column stackable grid",
        div(class = "column",
            tags$strong("What the prediction means"),
            p(style = "margin-top: 0.5em;",
              "The predicted duration represents expected time from study start to 
             completion date, based on patterns in ",
              format(app_info$n_train, big.mark = ","), " completed oncology trials that started
             in 2015 or earlier.")),
        div(class = "column",
            tags$strong("Prediction timepoint"),
            p(style = "margin-top: 0.5em;",
              "Inputs are final, reported values (actual enrollment, sites, countries), 
             which are only known once a trial is under way or finished. For planning, 
             treat them as your best estimates.")),
        div(class = "column",
            tags$strong("Limitations"),
            p(style = "margin-top: 0.5em;",
              "Protocol complexity, patient availability, and site performance are 
             not captured. Treat predictions as directional estimates."))
      )
    )
  )
}

# 1st, 2nd, 3rd, 4th ... 11th, 12th, 13th, 21st
ordinal <- function(n) {
  suffix <- if (n %% 100 %in% 11:13) "th" else switch(as.character(n %% 10), "1" = "st", "2" = "nd", "3" = "rd", "th")
  paste0(n, suffix)
}

mod_forecaster_server <- function(id, trials_data, ref_stats, phase_levels, sponsor_levels) {
  moduleServer(id, function(input, output, session) {
    
    # Store prediction
    pred_val <- reactiveVal(NULL)
    
    # Input checks: blank, non-integer or out-of-range values block prediction
    input_problems <- reactive({
      chk <- function(x, label, lo, hi) {
        if (is.null(x) || is.na(x)) return(paste0(label, " is required."))
        if (x != round(x)) return(paste0(label, " must be a whole number."))
        if (x < lo || x > hi) return(paste0(label, " must be between ", format(lo, big.mark = ","),
                                            " and ", format(hi, big.mark = ","), "."))
        NULL
      }
      probs <- c(
        chk(input$enrollment, "Enrollment", 5, 10000),
        chk(input$sites, "Number of sites", 1, 1000),
        chk(input$countries, "Number of countries", 1, 45),
        chk(input$reg_to_start, "Registration-to-start lag", -1825, 1500)
      )
      if (is.null(probs) && input$countries > input$sites)
        probs <- "Number of countries cannot exceed number of sites."
      probs
    })

    output$input_errors <- renderUI({
      probs <- input_problems()
      if (length(probs) == 0) return(NULL)
      div(class = "ui negative message", style = "margin-bottom: 1em;",
          tags$ul(class = "list", lapply(probs, tags$li)))
    })

    # Clear a stale prediction when inputs change
    observeEvent(list(input$phase, input$sponsor, input$enrollment, input$sites,
                      input$countries, input$reg_to_start),
                 pred_val(NULL), ignoreInit = TRUE)

    # Button click: predict with the saved Elastic Net workflow
    observeEvent(input$predict, {
      if (length(input_problems()) > 0) return()
      
      # Option A: Pass RAW inputs - recipe handles all transforms
      new_data <- tibble::tibble(
        enrollment_count = as.numeric(input$enrollment),
        n_sites = as.numeric(input$sites),
        n_countries = as.numeric(input$countries),
        registration_to_start = as.numeric(input$reg_to_start),
        phase_simple = factor(input$phase, levels = phase_levels),
        sponsor_simple = factor(input$sponsor, levels = sponsor_levels)
      )
      
      # Get prediction from real model (log scale)
      pred_log <- predict(final_workflow, new_data)$.pred
      
      # Back-transform to months
      pred_months <- exp(pred_log)
      
      # Bound to the observed range of the modeling cohort (6 months to 15 years)
      capped <- pred_months > 180 || pred_months < 6
      pred_months <- max(6, min(180, pred_months))
      
      pred_val(structure(pred_months, capped = capped))
    })
    
    # Main prediction display
    output$prediction_result <- renderUI({
      pred <- pred_val()
      if (is.null(pred)) {
        div(style = "text-align: center; padding: 2em; color: #666;",
            icon("calculator", style = "font-size: 2.5em; opacity: 0.3;"),
            p(style = "margin-top: 0.5em;", "Click 'Generate Prediction'"))
      } else {
        capped <- isTRUE(attr(pred, "capped"))
        pred <- as.numeric(pred)
        band <- pmin(180, pred * exp(ref_stats$resid_q))  # 10%, 25%, 75%, 90%; capped at 15 years
        div(class = "prediction-result",
            div(class = "prediction-value", format_duration(pred)),
            div(class = "prediction-label", "Model Estimate"),
            div(style = "margin-top: 0.5em; font-size: 0.9em; opacity: 0.85;",
                paste0("(", round(pred, 1), " months)")),
            if (capped) div(style = "margin-top: 0.5em; font-size: 0.85em;",
                            icon("exclamation circle"),
                            " Estimate capped at the app's 6–180 month display limits; these inputs push the model outside them."),
            div(style = "margin-top: 1em; font-size: 0.9em; line-height: 1.5;",
                tags$strong(paste0(round(band[1]), "–", round(band[4]), " months")),
                " likely range",
                br(),
                span(style = "font-size: 0.85em; opacity: 0.85;",
                     paste0("80% of 2016–2018 test trials finished within this range of their ",
                            "estimate (middle half: ", round(band[2]), "–", round(band[3]),
                            " months). A rough guide: the range is pooled over all test trials, ",
                            "and long recent trials are missing from them, so the upper end is ",
                            "likely too low."))))
      }
    })
    
    # Comparison section
    output$comparison <- renderUI({
      pred <- pred_val()
      if (is.null(pred)) {
        p(style = "color: #666;", "Generate a prediction to see comparison")
      } else {
        # Compare against the modeling cohort (started <= 2018), not the full
        # registry pull, whose 2019+ trials are right-truncated
        percentile <- round(mean(ref_stats$duration_values <= pred, na.rm = TRUE) * 100, 0)
        vs_median <- round(((pred / ref_stats$duration_median) - 1) * 100, 0)

        if (percentile < 50) {
          interp <- "shorter than the median historical trial"
          interp_color <- colors$success
        } else {
          interp <- "longer than the median historical trial"
          interp_color <- colors$warning
        }
        
        tidy_text(div(
          # Percentile bar
          div(style = "margin-bottom: 1em;",
              div(style = "display: flex; justify-content: space-between; font-size: 0.85em;",
                  span("Shorter"), span("Longer")),
              div(style = paste0("height: 8px; background: ", colors$border, "; border-radius: 4px; position: relative; margin-top: 0.3em;"),
                  div(style = paste0("position: absolute; left: ", percentile, "%; top: -4px; width: 14px; height: 14px; background: ", colors$primary, "; border-radius: 50%; transform: translateX(-50%); border: 2px solid white;"))),
              div(style = "text-align: center; font-size: 0.85em; margin-top: 0.5em;",
                  paste0(ordinal(percentile), " percentile"))),
          
          # Stats row
          div(class = "ui two column grid", style = "margin-top: 1em;",
              div(class = "column", style = "text-align: center;",
                  div(style = "font-size: 1.4em; font-weight: 600;", paste0(ifelse(vs_median > 0, "+", ""), vs_median, "%")),
                  div(style = paste0("font-size: 0.8em; color: ", colors$text_muted), "vs. median duration")),
              div(class = "column", style = "text-align: center;",
                  div(style = "font-size: 1.4em; font-weight: 600;", paste0(round(ref_stats$duration_median, 0), " mo")),
                  div(style = paste0("font-size: 0.8em; color: ", colors$text_muted),
                      paste0("median, trials started ", ref_stats$baseline_cutoff, " or earlier")))),
          
          # Interpretation
          div(style = paste0("margin-top: 1em; padding: 0.7em; background: ", colors$background, "; border-radius: 6px; border-left: 3px solid ", interp_color, ";"),
              p(style = "margin: 0; font-size: 0.9em;", "This prediction is ", tags$strong(interp), "."))
        ))
      }
    })
    
    # Trial profile
    output$profile <- renderUI({
      req(input$enrollment, input$sites, input$countries)
      validate(need(length(input_problems()) == 0, "Fix the inputs to see the profile."))
      div(
        tags$table(class = "ui very basic compact table", style = "width: 100%;",
                   tags$tbody(
                     tags$tr(tags$td("Enrollment per site"), tags$td(style = "text-align: right;", paste0(round(input$enrollment / input$sites, 1), " patients/site"))),
                     tags$tr(tags$td("Study type"), tags$td(style = "text-align: right;", ifelse(input$sites > 1, "Multi-site", "Single-site"))),
                     tags$tr(tags$td("Geographic scope"), tags$td(style = "text-align: right;", ifelse(input$countries > 1, "Multinational", "Single-country"))),
                     tags$tr(tags$td("Sites per country"), tags$td(style = "text-align: right;", paste0(round(input$sites / input$countries, 1), " avg")))
                   )
        )
      )
    })
    
  })
}