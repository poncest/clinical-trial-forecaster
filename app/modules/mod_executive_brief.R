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
            p("Trials that take longer to activate after registration tend to run longer overall. 
               This metric captures operational readiness and startup complexity, explaining more 
               variance than enrollment size or site count.")
          ),
          
          # Finding 2
          div(
            class = "insight-box",
            div(class = "insight-title", "Site Complexity Matters More Than Raw Count"),
            p("The enrollment-per-site ratio (burden per site) is a stronger predictor than 
               total sites alone. High-burden sites correlate with extended timelines, 
               reflecting recruitment challenges.")
          ),
          
          # Finding 3
          div(
            class = "insight-box",
            div(class = "insight-title", "Phase and Sponsor Type Show Expected Patterns"),
            p("Phase 3 trials run longer than Phase 2, and industry-sponsored trials differ 
               from academic/government studies. These align with domain knowledge and 
               provide model validation.")
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
                tags$td("Random Forest (500 trees)")
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
                tags$td("mtry=8, min_n=5 (tuned)")
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
         icon("target"), " Strategic Application"),
      
      div(
        class = "ui three column stackable grid",
        
        div(
          class = "column",
          h4(style = paste0("color: ", colors$accent), icon("calendar alternate"), " Portfolio Planning"),
          p("Use predictions to estimate resource requirements and timeline dependencies 
             across multiple concurrent trials. Identify high-risk studies early.")
        ),
        
        div(
          class = "column",
          h4(style = paste0("color: ", colors$accent), icon("dollar sign"), " Budget Forecasting"),
          p("Duration estimates feed directly into cost projections. Longer trials mean 
             extended site costs, monitoring, and overhead.")
        ),
        
        div(
          class = "column",
          h4(style = paste0("color: ", colors$accent), icon("exclamation triangle"), " Risk Assessment"),
          p("Trials predicted to run significantly longer than planned warrant closer 
             scrutiny of enrollment assumptions and operational readiness.")
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