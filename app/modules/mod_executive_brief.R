# =============================================================================
# Module: Executive Brief
# =============================================================================
# Purpose: High-level model summary, key metrics, and strategic insights
# Stage 3: Updated for Elastic Net model and time-based validation
# =============================================================================

mod_executive_brief_ui <- function(id) {
  ns <- NS(id)
  
  div(
    # Header
    div(
      class = "tab-header",
      h2(class = "tab-title", "Executive Brief"),
      p(class = "tab-subtitle", 
        "Clinical trial duration model trained on ",
        format(app_info$n_train, big.mark = ","), " oncology trials (started 2015 or earlier) and tested on ",
        format(app_info$n_test, big.mark = ","), " later trials (2016-2018)")
    ),
    
    # KPI Row
    div(
      class = "ui four column stackable grid",
      style = "margin-bottom: 2em;",
      
      # KPI 1: Model R²
      div(
        class = "column",
        div(
          class = "kpi-card",
          div(class = "kpi-value", sprintf("%.2f", model_metrics$r_squared)),
          div(class = "kpi-label", "R² on Later Trials"),
          div(class = "kpi-context",
              sprintf("vs %.2f on a random split", model_metrics$random_split_r_squared))
        )
      ),
      
      # KPI 2: Median Error
      div(
        class = "column",
        div(
          class = "kpi-card",
          div(class = "kpi-value", paste0(model_metrics$median_abs_error_months, " mo")),
          div(class = "kpi-label", "Median Absolute Error"),
          div(class = "kpi-context", "Half of test predictions were off by less than this")
        )
      ),
      
      # KPI 3: Training Data
      div(
        class = "column",
        div(
          class = "kpi-card",
          div(class = "kpi-value", format(app_info$n_train, big.mark = ",")),
          div(class = "kpi-label", "Training Trials"),
          div(class = "kpi-context",
              paste0("Started 2015 or earlier; tested on ", format(app_info$n_test, big.mark = ",")))
        )
      ),
      
      # KPI 4: Avg Duration
      div(
        class = "column",
        div(
          class = "kpi-card",
          div(class = "kpi-value", uiOutput(ns("avg_duration"), inline = TRUE)),
          div(class = "kpi-label", "Median Trial Duration"),
          div(class = "kpi-context",
              paste0("Trials started ", ref_stats$baseline_cutoff, " or earlier"))
        )
      )
    ),
    
    # Two-column layout: Key Findings + Model Summary
    div(
      class = "ui two column stackable grid",
      
      # Left: Key Findings
      div(
        class = "column",
        div(
          class = "ui segment",
          h3(style = paste0("color: ", colors$primary_dark, "; margin-top: 0;"), 
             icon("lightbulb outline"), " Key Findings"),
          
          # Finding 1
          div(
            class = "insight-box",
            div(class = "insight-title", "Sponsor Type Shows the Strongest Association"),
            p(paste0("Among modeled trials, median duration is ", sponsor_medians[["Industry"]], " months for industry sponsors versus ", sponsor_medians[["Academic/Other"]], " for academic, ", sponsor_medians[["NIH"]], " for NIH and ", sponsor_medians[["Government/Network"]], " for government or cooperative-group trials. This may reflect different designs, endpoints or funding cycles; causality cannot be established from observational data."))
          ),
          
          # Finding 2
          div(
            class = "insight-box",
            div(class = "insight-title", "Most of the Signal Is Known Only in Hindsight"),
            p(paste0(
              "Inputs known only once a trial is under way (final enrollment, sites, countries) carry ",
              "most of the signal. With only what is known at study start (phase, sponsor, ",
              "registration-to-start lag), R² falls from ",
              sprintf("%.2f", prestart_r2("all (published model)", "random")), " to ",
              sprintf("%.2f", prestart_r2("pre-start only", "random")),
              " on a random split, and to ", sprintf("%.2f", prestart_r2("pre-start only", "time-based")),
              " on later trials (worse than predicting the average)."
            ))
          ),
          
          # Finding 3
          div(
            class = "insight-box",
            div(class = "insight-title", "Limited Forecasting Power"),
            p(paste0("Registry features explain about ", round(100 * model_metrics$random_split_r_squared), "% of duration variance on a random split, but about ", round(100 * model_metrics$r_squared), "% for trials started after the training period (about ", round(100 * model_metrics$random_trunc_r_squared), "% on a random split scored on the same truncated test window, so part of the drop is the window itself). Protocol complexity, endpoint strategy and operational factors not captured in public data drive most of the variation."))
          )
        )
      ),
      
      # Right: Model Summary
      div(
        class = "column",
        div(
          class = "ui segment",
          h3(style = paste0("color: ", colors$primary_dark, "; margin-top: 0;"), 
             icon("cogs"), " Model Summary"),
          
          # Model details table
          tags$table(
            class = "ui very basic table",
            style = "margin-top: 1em;",
            tags$tbody(
              tags$tr(
                tags$td(tags$strong("Algorithm")),
                tags$td(paste0("Elastic Net (mixture ", model_metrics$mixture, ")"))
              ),
              tags$tr(
                tags$td(tags$strong("Features")),
                tags$td(paste0("6 inputs (", nrow(feature_importance), " model terms)"))
              ),
              tags$tr(
                tags$td(tags$strong("Training Method")),
                tags$td("10-fold cross-validation")
              ),
              tags$tr(
                tags$td(tags$strong("Validation")),
                tags$td(paste0("Time-based (train ≤", model_metrics$train_cutoff, ", test ",
                               model_metrics$test_start, "–", model_metrics$test_end, ")"))
              ),
              tags$tr(
                tags$td(tags$strong("Test Performance")),
                tags$td(sprintf("R² = %.2f, RMSE = %.3f (log scale)",
                                model_metrics$r_squared, model_metrics$rmse))
              )
            )
          ),
          
          hr(),
          
          h4(style = paste0("color: ", colors$primary_dark), "Top Predictors"),
          
          # Mini feature importance
          girafeOutput(ns("mini_importance"), height = "200px")
        )
      )
    ),
    
    # Strategic Context
    div(
      class = "ui segment",
      style = "margin-top: 1.5em;",
      
      h3(style = paste0("color: ", colors$primary_dark, "; margin-top: 0;"), 
         icon("target"), " What This Analysis Is Good For"),
      
      div(
        class = "ui three column stackable grid",
        
        div(
          class = "column",
          h4(style = paste0("color: ", colors$accent), icon("check circle outline"), " Validation Practice"),
          p("A worked example of why validation must match intended use: the same model 
             scores R² ", sprintf("%.2f", model_metrics$random_split_r_squared),
            " on a random split and ", sprintf("%.2f", model_metrics$r_squared),
            " on later trials, and a truncation-matched check shows how much of that gap ",
            "comes from the test window rather than time. It also shows why independent review matters: an earlier 
             version's R² of 0.84 came from a date-parsing bug (see Methods & Data).")
        ),
        
        div(
          class = "column",
          h4(style = paste0("color: ", colors$accent), icon("history"), " Historical Patterns"),
          p("Describe how registry characteristics relate to duration in completed trials, 
             e.g., longer durations for non-industry sponsors. Associations only; they 
             should not be read as causes or used to set timelines.")
        ),
        
        div(
          class = "column",
          h4(style = paste0("color: ", colors$accent), icon("exclamation triangle"), " Uncertainty Awareness"),
          p("Show how wide honest ranges are: 80% of recent test trials fell between ",
            sprintf("%.2f", exp(ref_stats$resid_q[[1]])), " and ",
            sprintf("%.2f", exp(ref_stats$resid_q[[4]])), " times their model estimate. 
             A planned timeline cannot be meaningfully confirmed or rejected with a band that wide.")
        )
      )
    ),
    
    # Disclaimer
    div(
      style = paste0("margin-top: 1.5em; padding: 1em; background: #FFF8E1; 
                      border-radius: 8px; border-left: 4px solid ", colors$warning, ";"),
      p(style = "margin: 0; color: #5D4037;",
        tags$strong("Note: "), 
        "This model provides directional estimates based on historical patterns in public 
         registry data. Actual trial duration depends on protocol-specific factors, 
         therapeutic area dynamics, and operational execution not captured in registry data.")
    )
  )
}

mod_executive_brief_server <- function(id, trials_data, model_metrics, ref_stats) {
  moduleServer(id, function(input, output, session) {
    
    # Average duration KPI
    output$avg_duration <- renderText({
      paste0(round(ref_stats$duration_median, 1), " mo")
    })
    
    # Mini feature importance chart (top 5)
    output$mini_importance <- renderGirafe({
      
      # Normalize column names (Elastic Net uses term/abs_estimate, XGBoost uses Variable/Importance)
      importance_data <- feature_importance
      if ("term" %in% names(importance_data)) {
        importance_data <- importance_data |>
          dplyr::rename(Variable = term, Importance = abs_estimate)
      }
      
      # Get top 5 features
      top_features <- importance_data |>
        head(5) |>
        mutate(
          feature_label = feature_label(Variable),
          feature_label = factor(feature_label, levels = rev(feature_label))
        )
      
      # Create chart
      p <- ggplot(top_features, aes(x = Importance, y = feature_label)) +
        geom_col_interactive(
          aes(
            tooltip = paste0(feature_label, "\nCoefficient: ", sprintf("%+.2f", estimate)),
            data_id = Variable
          ),
          fill = colors$primary,
          width = 0.7
        ) +
        scale_x_continuous(expand = expansion(mult = c(0, 0.1))) +
        labs(x = NULL, y = NULL) +
        theme_minimal(base_size = 11) +
        theme(
          panel.grid.major.y = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text.y = element_text(color = colors$text_primary, size = 10),
          axis.text.x = element_blank(),
          plot.margin = ggplot2::margin(5, 10, 5, 5, unit = "pt")
        )
      
      girafe(
        ggobj = p,
        width_svg = 5,
        height_svg = 2.5,
        options = list(
          opts_hover(css = "fill:#023E8A;"),
          opts_tooltip(css = "background:white;padding:8px;border-radius:4px;box-shadow:0 2px 6px rgba(0,0,0,0.1);"),
          opts_sizing(rescale = TRUE)
        )
      )
    })
    
  })
}