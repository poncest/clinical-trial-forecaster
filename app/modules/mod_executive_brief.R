# =============================================================================
# Module: Executive Brief
# =============================================================================
# Purpose: High-level model summary, key metrics, and strategic insights
# =============================================================================

mod_executive_brief_ui <- function(id) {
  ns <- NS(id)
  
  div(
    # Header
    div(
      class = "tab-header",
      h2(class = "tab-title", "Executive Brief"),
      p(class = "tab-subtitle", 
        "Clinical trial duration forecasting model trained on ", 
        format(app_info$n_trials, big.mark = ","), " oncology trials")
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
          div(class = "kpi-value", paste0(model_metrics$r_squared * 100, "%")),
          div(class = "kpi-label", "Variance Explained (R²)"),
          div(class = "kpi-context", "Test set performance")
        )
      ),
      
      # KPI 2: Median Error
      div(
        class = "column",
        div(
          class = "kpi-card",
          div(class = "kpi-value", paste0(model_metrics$median_abs_error_months, " mo")),
          div(class = "kpi-label", "Median Prediction Error"),
          div(class = "kpi-context", "50% of predictions within this range")
        )
      ),
      
      # KPI 3: Training Data
      div(
        class = "column",
        div(
          class = "kpi-card",
          div(class = "kpi-value", format(app_info$n_trials, big.mark = ",")),
          div(class = "kpi-label", "Training Trials"),
          div(class = "kpi-context", "Completed oncology studies")
        )
      ),
      
      # KPI 4: Avg Duration
      div(
        class = "column",
        div(
          class = "kpi-card",
          div(class = "kpi-value", uiOutput(ns("avg_duration"), inline = TRUE)),
          div(class = "kpi-label", "Average Trial Duration"),
          div(class = "kpi-context", "Historical baseline")
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
            div(class = "insight-title", "Registration-to-Start Time is the Top Predictor"),
            p("Trials with longer registration-to-start lag are strongly associated with longer 
               overall durations in this dataset. This may reflect a mix of startup complexity, 
               site activation delays, or registry data update patterns. Note: this feature 
               requires prediction at or after study start.")
          ),
          
          # Finding 2
          div(
            class = "insight-box",
            div(class = "insight-title", "Site Burden Ratio Shows Strong Association"),
            p("The enrollment-per-site ratio is a stronger predictor than total sites alone. 
               Higher patient burden per site is associated with longer timelines, possibly 
               due to recruitment capacity constraints—though causality cannot be established 
               from this observational data.")
          ),
          
          # Finding 3
          div(
            class = "insight-box",
            div(class = "insight-title", "Phase and Sponsor Type Show Expected Patterns"),
            p("Phase 3 trials run longer than Phase 2, and industry-sponsored trials differ 
               from academic/government studies. These associations align with domain knowledge 
               and provide face validity for the model.")
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
                tags$td("XGBoost (100 trees)")
              ),
              tags$tr(
                tags$td(tags$strong("Features")),
                tags$td("9 predictors")
              ),
              tags$tr(
                tags$td(tags$strong("Training Method")),
                tags$td("10-fold cross-validation")
              ),
              tags$tr(
                tags$td(tags$strong("Hyperparameters")),
                tags$td("tree_depth=3, learn_rate=0.1 (tuned)")
              ),
              tags$tr(
                tags$td(tags$strong("Test Performance")),
                tags$td(paste0("R²=", model_metrics$r_squared, 
                               ", RMSE=", round(model_metrics$rmse, 3)))
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
         icon("target"), " Potential Applications"),
      
      div(
        class = "ui three column stackable grid",
        
        div(
          class = "column",
          h4(style = paste0("color: ", colors$accent), icon("calendar alternate"), " Portfolio Planning"),
          p("Use predictions as one input to estimate resource requirements and timeline 
             dependencies. Flag studies with longer predicted durations for additional 
             review—not as go/no-go decisions.")
        ),
        
        div(
          class = "column",
          h4(style = paste0("color: ", colors$accent), icon("dollar sign"), " Budget Scenarios"),
          p("Duration estimates can inform cost projections. Consider generating scenarios 
             across a range of durations (e.g., predicted ±6 months) rather than relying 
             on point estimates.")
        ),
        
        div(
          class = "column",
          h4(style = paste0("color: ", colors$accent), icon("exclamation triangle"), " Assumption Testing"),
          p("Compare predictions to planned timelines. Large discrepancies may indicate 
             optimistic assumptions worth reviewing—but model limitations mean discrepancies 
             don't necessarily indicate problems.")
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
      avg_months <- round(ref_stats$duration_mean, 1)
      paste0(avg_months, " mo")
    })
    
    # Mini feature importance chart (top 5)
    output$mini_importance <- renderGirafe({
      
      # Get top 5 features
      top_features <- feature_importance |>
        head(5) |>
        mutate(
          feature_label = case_when(
            Variable == "registration_to_start" ~ "Reg-to-Start Time",
            Variable == "log_sites" ~ "Number of Sites",
            Variable == "log_enrollment" ~ "Enrollment Size",
            Variable == "log_enrollment_per_site" ~ "Enrollment/Site",
            Variable == "n_countries" ~ "Countries",
            TRUE ~ Variable
          ),
          feature_label = factor(feature_label, levels = rev(feature_label))
        )
      
      # Create chart
      p <- ggplot(top_features, aes(x = Importance, y = feature_label)) +
        geom_col_interactive(
          aes(
            tooltip = paste0(feature_label, "\nImportance: ", round(Importance, 0)),
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