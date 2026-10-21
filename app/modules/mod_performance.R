# =============================================================================
# Module: Model Performance
# =============================================================================
# Purpose: Model diagnostics, validation metrics, and comparison
# Stage 3: Updated for Elastic Net and time-based validation
# =============================================================================

mod_performance_ui <- function(id) {
  ns <- NS(id)
  
  div(
    # Header
    div(
      class = "tab-header",
      h2(class = "tab-title", icon("check circle"), " Model Performance"),
      p(class = "tab-subtitle", 
        "Validation metrics, diagnostic plots, and model comparison")
    ),
    
    # Metrics Summary
    div(
      class = "ui four column stackable grid",
      style = "margin-bottom: 2em;",
      
      div(
        class = "column",
        div(
          class = "kpi-card",
          div(class = "kpi-value", 
              style = paste0("color: ", colors$primary),
              sprintf("%.2f", model_metrics$r_squared)),
          div(class = "kpi-label", "R² (1 − SSE/SST)"),
          div(class = "kpi-context", sprintf("Test set; squared correlation %.2f", model_metrics$r_squared_cor))
        )
      ),
      
      div(
        class = "column",
        div(
          class = "kpi-card",
          div(class = "kpi-value", round(model_metrics$rmse, 3)),
          div(class = "kpi-label", "RMSE (log scale)"),
          div(class = "kpi-context", "Lower is better")
        )
      ),
      
      div(
        class = "column",
        div(
          class = "kpi-card",
          div(class = "kpi-value", paste0(model_metrics$median_abs_error_months, " mo")),
          div(class = "kpi-label", "Median Absolute Error"),
          div(class = "kpi-context", "In months")
        )
      ),
      
      div(
        class = "column",
        div(
          class = "kpi-card",
          div(class = "kpi-value", paste0(model_metrics$mean_abs_error_months, " mo")),
          div(class = "kpi-label", "Mean Absolute Error"),
          div(class = "kpi-context", "In months")
        )
      )
    ),
    
    # Diagnostic plots row
    div(
      class = "ui two column stackable grid",
      
      # Actual vs Predicted
      div(
        class = "column",
        div(
          class = "ui segment",
          h4(style = paste0("color: ", colors$primary_dark, "; margin-top: 0;"),
             "Actual vs Predicted Duration"),
          girafeOutput(ns("actual_vs_predicted"), height = "350px"),
          p(style = paste0("font-size: 0.85em; color: ", colors$text_muted, "; margin-top: 0.5em;"),
            local({
              pr <- quantile(test_predictions$predicted_months, c(0.1, 0.9))
              ac <- quantile(test_predictions$actual_months, c(0.1, 0.9))
              paste0(
                "Points on the diagonal would be perfect predictions. Predictions sit in a narrow band ",
                "(middle 80%: ", round(pr[1]), "–", round(pr[2]), " months) while actual durations ",
                "spread much wider (", round(ac[1]), "–", round(ac[2]), " months): the model explains ",
                "little of the variation between trials."
              )
            }))
        )
      ),
      
      # Residual distribution
      div(
        class = "column",
        div(
          class = "ui segment",
          h4(style = paste0("color: ", colors$primary_dark, "; margin-top: 0;"),
             "Prediction Error Distribution"),
          girafeOutput(ns("residual_dist"), height = "350px"),
          p(style = paste0("font-size: 0.85em; color: ", colors$text_muted, "; margin-top: 0.5em;"),
            local({
              err <- test_predictions$residual_months  # actual minus predicted
              paste0(
                "Error = actual minus predicted. The median error is ",
                sprintf("%.1f", median(err, na.rm = TRUE)), " months and ",
                round(100 * mean(err < 0, na.rm = TRUE)), "% of 2016-2018 test trials ",
                "finished sooner than predicted. On the log scale the bias is larger: test trials ran ",
                "on average about ", model_metrics$pct_shorter_than_predicted, "% shorter than predicted, ",
                "consistent with the test window excluding long trials that had not finished by the ",
                "data pull. Both the bias and the wide spread lower R²."
              )
            }))
        )
      )
    ),
    
    # Model comparison section
    div(
      class = "ui segment",
      style = "margin-top: 1.5em;",
      
      h3(style = paste0("color: ", colors$primary_dark, "; margin-top: 0;"),
         icon("balance scale"), " Model Comparison"),
      p(style = paste0("color: ", colors$text_secondary),
        paste("Four model types were compared using 10-fold cross-validation within the training
         data (trials started 2015 or earlier). All metrics in a row come from the same
         configuration (the one with the best CV RMSE); R² (CV) here is the squared correlation.",
         selection_note, "The 2016-2018 test set was not used for model selection; it was
         used for the final evaluation and to size the forecaster's likely range.")),
      
      reactableOutput(ns("model_comparison_table"))
    ),
    
    # Error analysis section
    div(
      class = "ui segment",
      style = "margin-top: 1.5em;",
      
      h3(style = paste0("color: ", colors$primary_dark, "; margin-top: 0;"),
         icon("search"), " Error Analysis"),
      
      div(
        class = "ui two column stackable grid",
        
        div(
          class = "column",
          h4("Error by Phase"),
          girafeOutput(ns("error_by_phase"), height = "250px")
        ),
        
        div(
          class = "column",
          h4("Error by Sponsor Type"),
          girafeOutput(ns("error_by_sponsor"), height = "250px"),
          local({
            n_by <- table(test_predictions$sponsor_simple)
            small <- names(n_by)[n_by < 30]
            if (length(small) > 0)
              p(style = paste0("font-size: 0.85em; color: ", colors$text_muted, ";"),
                paste0(paste(small, collapse = " and "), " have fewer than 30 test trials; ",
                       "treat their bars as noisy."))
          })
        )
      )
    ),
    
    # Methodology notes
    div(
      class = "ui segment",
      style = "margin-top: 1.5em;",
      
      h3(style = paste0("color: ", colors$primary_dark, "; margin-top: 0;"),
         icon("info circle"), " Methodology Notes"),
      
      div(
        class = "ui three column stackable grid",
        
        div(
          class = "column",
          h4("Training Approach"),
          tags$ul(
            tags$li("Time-based train/test split"),
            tags$li("Train: ≤2015, Test: 2016-2018"),
            tags$li("10-fold cross-validation (within training)"),
            tags$li("Excluded 2019+ (only shorter trials had finished)")
          )
        ),
        
        div(
          class = "column",
          h4("Target Transform"),
          tags$ul(
            tags$li("Log-transformed duration"),
            tags$li("Normalizes right-skewed distribution"),
            tags$li("Stabilizes variance"),
            tags$li("Predictions back-transformed to months")
          )
        ),
        
        div(
          class = "column",
          h4("Limitations"),
          tags$ul(
            tags$li("Oncology-specific model"),
            tags$li(sprintf("Limited prospective accuracy (R² ≈ %.2f)", model_metrics$r_squared)),
            tags$li("Registry data lacks protocol details"),
            tags$li("Test window excludes long, unfinished trials")
          )
        )
      )
    )
  )
}

mod_performance_server <- function(id, test_predictions, model_comparison, model_metrics) {
  moduleServer(id, function(input, output, session) {
    
    # Actual vs Predicted scatter plot
    output$actual_vs_predicted <- renderGirafe({
      
      plot_data <- test_predictions |>
        mutate(
          row_id = row_number(),
          actual_months = exp(log_duration),
          predicted_months = exp(.pred),
          error_months = actual_months - predicted_months
        )
      
      p <- ggplot(plot_data, aes(x = actual_months, y = predicted_months)) +
        geom_abline(slope = 1, intercept = 0, color = colors$text_muted, linetype = "dashed") +
        geom_point_interactive(
          aes(
            tooltip = paste0(
              "Actual: ", round(actual_months, 1), " months\n",
              "Predicted: ", round(predicted_months, 1), " months\n",
              "Error: ", round(error_months, 1), " months"
            ),
            data_id = row_id
          ),
          color = colors$primary,
          alpha = 0.5,
          size = 2
        ) +
        scale_x_continuous(limits = c(0, 150), labels = comma_format()) +
        scale_y_continuous(limits = c(0, 150), labels = comma_format()) +
        labs(
          x = "Actual Duration (months)",
          y = "Predicted Duration (months)"
        ) +
        theme_minimal(base_size = 11) +
        theme(
          panel.grid.minor = element_blank()
        ) +
        coord_fixed()
      
      girafe(
        ggobj = p,
        width_svg = 5,
        height_svg = 4.5,
        options = list(
          opts_hover(css = "fill:#023E8A;fill-opacity:1;r:4;"),
          opts_tooltip(css = "background:white;padding:8px;border-radius:4px;box-shadow:0 2px 6px rgba(0,0,0,0.1);"),
          opts_sizing(rescale = TRUE)
        )
      )
    })
    
    # Residual distribution
    output$residual_dist <- renderGirafe({
      
      plot_data <- test_predictions |>
        mutate(
          actual_months = exp(log_duration),
          predicted_months = exp(.pred),
          error_months = actual_months - predicted_months
        )
      
      p <- ggplot(plot_data, aes(x = error_months)) +
        geom_histogram_interactive(
          aes(
            tooltip = after_stat(paste0("Error range: ", round(xmin, 0), " to ", round(xmax, 0), " months\nCount: ", count))
          ),
          fill = colors$primary,
          color = "white",
          bins = 30
        ) +
        geom_vline(xintercept = 0, color = colors$warning, linetype = "dashed", linewidth = 1) +
        geom_vline(
          xintercept = median(plot_data$error_months),
          color = colors$accent,
          linetype = "solid",
          linewidth = 0.8
        ) +
        annotate(
          "text",
          x = median(plot_data$error_months),
          y = Inf,
          label = paste0("Median: ", round(median(plot_data$error_months), 1)),
          vjust = 2,
          hjust = -0.1,
          color = colors$accent,
          fontface = "bold",
          size = 3
        ) +
        scale_x_continuous(limits = c(-1, 1) * ceiling(max(abs(test_predictions$residual_months)) / 10) * 10) +
        labs(
          x = "Prediction Error (months)",
          y = "Number of Trials"
        ) +
        theme_minimal(base_size = 11) +
        theme(
          panel.grid.minor = element_blank()
        )
      
      girafe(
        ggobj = p,
        width_svg = 5,
        height_svg = 4.5,
        options = list(
          opts_hover(css = "fill:#023E8A;"),
          opts_tooltip(css = "background:white;padding:8px;border-radius:4px;box-shadow:0 2px 6px rgba(0,0,0,0.1);"),
          opts_sizing(rescale = TRUE)
        )
      )
    })
    
    # Model comparison table
    output$model_comparison_table <- renderReactable({
      
      comp_data <- model_comparison |>
        mutate(
          Model = case_when(
            model == "random_forest" ~ "Random Forest",
            model == "Random Forest" ~ "Random Forest",
            model == "xgboost" ~ "XGBoost",
            model == "XGBoost" ~ "XGBoost",
            model == "elastic_net" ~ "Elastic Net",
            model == "Elastic Net" ~ "Elastic Net",
            model == "linear_reg" ~ "Linear Regression",
            model == "Linear Regression" ~ "Linear Regression",
            TRUE ~ model
          )
        ) |>
        arrange(rmse) |>
        select(
          Model,
          `RMSE (CV)` = rmse,
          `± 1 SE` = rmse_std_err,
          `MAE (CV)` = mae,
          `R² (CV, sq. corr.)` = rsq
        )
      
      reactable(
        comp_data,
        columns = list(
          Model = colDef(
            width = 150,
            cell = function(value, index) {
              if (value == app_info$model_type) {
                div(
                  span(value, style = "font-weight: 600;"),
                  span(
                    style = paste0(
                      "margin-left: 8px; padding: 2px 8px; background: ", colors$success, 
                      "; color: white; border-radius: 4px; font-size: 0.75em;"
                    ),
                    "Selected"
                  )
                )
              } else {
                value
              }
            }
          ),
          `RMSE (CV)` = colDef(
            format = colFormat(digits = 3),
            style = function(value) {
              if (value == min(comp_data$`RMSE (CV)`)) {
                list(fontWeight = 600, color = colors$success)
              }
            }
          ),
          `± 1 SE` = colDef(format = colFormat(digits = 3)),
          `MAE (CV)` = colDef(format = colFormat(digits = 3)),
          `R² (CV, sq. corr.)` = colDef(
            format = colFormat(digits = 3),
            style = function(value) {
              if (value == max(comp_data$`R² (CV, sq. corr.)`)) {
                list(fontWeight = 600, color = colors$success)
              }
            }
          )
        ),
        bordered = FALSE,
        striped = TRUE,
        highlight = TRUE,
        style = list(fontSize = "0.95em")
      )
    })
    
    # Error by phase
    output$error_by_phase <- renderGirafe({
      
      # Compute actual error by phase from test predictions
      phase_error <- test_predictions |>
        group_by(phase_simple) |>
        summarise(
          median_error = median(abs_error_months, na.rm = TRUE),
          n = n(),
          .groups = "drop"
        ) |>
        filter(!is.na(phase_simple)) |>
        mutate(
          phase = factor(phase_simple, 
                         levels = rev(c("Phase 2", "Phase 1/2", "Phase 2/3", "Phase 3")))
        )
      
      n_lookup <- setNames(phase_error$n, as.character(phase_error$phase))
      p <- ggplot(phase_error, aes(x = phase, y = median_error)) +
        geom_col_interactive(
          aes(
            tooltip = paste0(phase_simple, " (n=", n, ")\nMedian Error: ", round(median_error, 1), " months"),
            data_id = phase_simple
          ),
          fill = colors$primary,
          width = 0.6
        ) +
        geom_hline(
          yintercept = model_metrics$median_abs_error_months,
          color = colors$warning,
          linetype = "dashed"
        ) +
        scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
        labs(x = NULL, y = "Median Absolute Error (months)") +
        scale_x_discrete(labels = function(x) paste0(x, " (n=", n_lookup[x], ")")) +
        coord_flip() +
        theme_minimal(base_size = 10) +
        theme(
          panel.grid.major.y = element_blank(),
          panel.grid.minor = element_blank()
        )
      
      girafe(
        ggobj = p,
        width_svg = 4.5,
        height_svg = 3,
        options = list(
          opts_hover(css = "fill:#023E8A;"),
          opts_tooltip(css = "background:white;padding:8px;border-radius:4px;"),
          opts_sizing(rescale = TRUE)
        )
      )
    })
    
    # Error by sponsor
    output$error_by_sponsor <- renderGirafe({
      
      # Compute actual error by sponsor from test predictions
      sponsor_error <- test_predictions |>
        group_by(sponsor_simple) |>
        summarise(
          median_error = median(abs_error_months, na.rm = TRUE),
          n = n(),
          .groups = "drop"
        ) |>
        filter(!is.na(sponsor_simple)) |>
        mutate(
          sponsor = factor(sponsor_simple, 
                           levels = rev(c("Industry", "Academic/Other", "NIH", "Government/Network")))
        )
      
      n_lookup <- setNames(sponsor_error$n, as.character(sponsor_error$sponsor))
      p <- ggplot(sponsor_error, aes(x = sponsor, y = median_error)) +
        geom_col_interactive(
          aes(
            tooltip = paste0(sponsor_simple, " (n=", n, ")\nMedian Error: ", round(median_error, 1), " months"),
            data_id = sponsor_simple
          ),
          fill = colors$primary,
          width = 0.6
        ) +
        geom_hline(
          yintercept = model_metrics$median_abs_error_months,
          color = colors$warning,
          linetype = "dashed"
        ) +
        scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
        labs(x = NULL, y = "Median Absolute Error (months)") +
        scale_x_discrete(labels = function(x) paste0(x, " (n=", n_lookup[x], ")")) +
        coord_flip() +
        theme_minimal(base_size = 10) +
        theme(
          panel.grid.major.y = element_blank(),
          panel.grid.minor = element_blank()
        )
      
      girafe(
        ggobj = p,
        width_svg = 4.5,
        height_svg = 3,
        options = list(
          opts_hover(css = "fill:#023E8A;"),
          opts_tooltip(css = "background:white;padding:8px;border-radius:4px;"),
          opts_sizing(rescale = TRUE)
        )
      )
    })
    
  })
}