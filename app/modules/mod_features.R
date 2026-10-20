# =============================================================================
# Module: Feature Explorer
# =============================================================================
# Purpose: Feature importance visualization and relationship exploration
# Stage 3: Updated to handle Elastic Net coefficient format
# =============================================================================

mod_features_ui <- function(id) {
  ns <- NS(id)
  
  div(
    # Header
    div(
      class = "tab-header",
      h2(class = "tab-title", icon("chart bar"), " Feature Explorer"),
      p(class = "tab-subtitle", 
        "Understand which trial characteristics most influence duration predictions")
    ),
    
    # Feature Importance Section
    div(
      class = "ui segment",
      
      h3(style = paste0("color: ", colors$primary_dark, "; margin-top: 0;"), 
         "Feature Importance"),
      p(style = paste0("color: ", colors$text_secondary),
        "Bars show the absolute size of each Elastic Net coefficient (numeric features standardized). Larger values
         mean the feature moves predictions more; they do not measure contribution to accuracy,
         and they indicate association, not causation."),
      
      girafeOutput(ns("importance_chart"), height = "400px")
    ),
    
    # Two-column: Feature descriptions + Distributions
    div(
      class = "ui two column stackable grid",
      style = "margin-top: 1.5em;",
      
      # Left: Feature Descriptions
      div(
        class = "column",
        div(
          class = "ui segment",
          h3(style = paste0("color: ", colors$primary_dark, "; margin-top: 0;"), 
             "Feature Descriptions"),
          
          reactableOutput(ns("feature_table"))
        )
      ),
      
      # Right: Distribution Explorer
      div(
        class = "column",
        div(
          class = "ui segment",
          h3(style = paste0("color: ", colors$primary_dark, "; margin-top: 0;"), 
             "Distribution Explorer"),
          p(style = paste0("font-size: 0.85em; color: ", colors$text_muted, ";"),
            paste0("Trials that started in ", ref_stats$baseline_cutoff, " or earlier (n = ",
                   format(ref_stats$baseline_n, big.mark = ","), "), the cohort used for modeling. ",
                   "Later trials are excluded because only the shortest had finished by the data pull.")),

          selectInput(
            ns("selected_feature"),
            label = "Select feature to explore:",
            choices = c(
              "Trial Duration" = "duration_months",
              "Enrollment (actual)" = "enrollment_count",
              "Number of Sites" = "n_sites",
              "Number of Countries" = "n_countries",
              "Registration to Start (days)" = "registration_to_start"
            ),
            selected = "duration_months",
            width = "100%"
          ),
          
          girafeOutput(ns("distribution_chart"), height = "280px"),
          
          uiOutput(ns("distribution_stats"))
        )
      )
    ),
    
    # Key Insights Section
    div(
      class = "ui segment",
      style = "margin-top: 1.5em;",
      
      h3(style = paste0("color: ", colors$primary_dark, "; margin-top: 0;"), 
         icon("lightbulb"), " Interpretation Guide"),
      
      div(
        class = "ui three column stackable grid",
        
        div(
          class = "column",
          div(
            class = "insight-box",
            div(class = "insight-title", "Sponsor Type Shows Strongest Signal"),
            p("Non-industry sponsors (NIH, Government, Academic) show the largest coefficients,
               suggesting these trials tend to run longer than industry-sponsored studies—
               though this association may reflect different trial designs rather than 
               operational efficiency.")
          )
        ),
        
        div(
          class = "column",
          div(
            class = "insight-box",
            div(class = "insight-title", "Log Transforms Help"),
            p("Sites and enrollment use log transforms because their relationship with 
               duration is non-linear. Doubling from 10 to 20 sites has more impact than 
               doubling from 100 to 200.")
          )
        ),
        
        div(
          class = "column",
          div(
            class = "insight-box",
            div(class = "insight-title", "Limited Predictive Power"),
            p("Despite clear associations, these features explain only ~4% of variance in 
               future trial durations. Protocol complexity, endpoints, and operational 
               factors not in the registry drive most of the variation.")
          )
        )
      )
    )
  )
}

mod_features_server <- function(id, trials_data, feature_importance) {
  moduleServer(id, function(input, output, session) {
    
    # Feature importance chart
    output$importance_chart <- renderGirafe({
      
      # Normalize column names (Elastic Net uses term/abs_estimate, XGBoost uses Variable/Importance)
      plot_data <- feature_importance
      if ("term" %in% names(plot_data)) {
        plot_data <- plot_data |>
          dplyr::rename(Variable = term, Importance = abs_estimate)
      }
      
      # Prepare data
      plot_data <- plot_data |>
        mutate(
          feature_label = case_when(
            Variable == "registration_to_start" ~ "Registration to Start Time",
            Variable == "log_sites" ~ "Number of Sites (log)",
            Variable == "log_enrollment" ~ "Enrollment Size (log)",
            Variable == "log_enrollment_per_site" ~ "Enrollment per Site (log)",
            Variable == "n_countries" ~ "Number of Countries",
            Variable == "is_multinational" ~ "Multinational Flag",
            Variable == "is_multisite" ~ "Multi-site Flag",
            Variable == "sponsor_simple_Government.Network" ~ "Sponsor: Government/Network",
            Variable == "sponsor_simple_Academic.Other" ~ "Sponsor: Academic/Other",
            Variable == "sponsor_simple_NIH" ~ "Sponsor: NIH",
            Variable == "phase_simple_Phase.3" ~ "Phase 3",
            Variable == "phase_simple_Phase.2.3" ~ "Phase 2/3",
            Variable == "phase_simple_Phase.1.2" ~ "Phase 1/2",
            TRUE ~ gsub("_", " ", Variable)
          ),
          feature_label = factor(feature_label, levels = rev(feature_label)),
          pct_of_max = Importance / max(Importance) * 100,
          category = case_when(
            grepl("sponsor|phase", Variable, ignore.case = TRUE) ~ "Categorical",
            TRUE ~ "Numeric"
          )
        )
      
      # Create chart
      p <- ggplot(plot_data, aes(x = Importance, y = feature_label)) +
        geom_col_interactive(
          aes(
            fill = category,
            tooltip = paste0(
              "<strong>", feature_label, "</strong><br>",
              "Coefficient: ", round(Importance, 3), "<br>",
              "% of max: ", round(pct_of_max, 1), "%"
            ),
            data_id = Variable
          ),
          width = 0.7
        ) +
        geom_text(
          aes(label = round(Importance, 2)),
          hjust = -0.1,
          size = 3,
          color = colors$text_secondary
        ) +
        scale_fill_manual(
          values = c("Numeric" = colors$primary, "Categorical" = colors$accent_light),
          name = "Feature Type"
        ) +
        scale_x_continuous(expand = expansion(mult = c(0, 0.15))) +
        labs(
          x = "Coefficient Magnitude",
          y = NULL,
          title = "Feature Importance (Elastic Net)"
        ) +
        theme_minimal(base_size = 12) +
        theme(
          panel.grid.major.y = element_blank(),
          panel.grid.minor = element_blank(),
          legend.position = "top",
          legend.justification = "left",
          plot.title = element_text(size = 14, face = "bold", color = colors$text_primary),
          axis.text.y = element_text(color = colors$text_primary)
        )
      
      girafe(
        ggobj = p,
        width_svg = 8,
        height_svg = 5,
        options = list(
          opts_hover(css = "fill-opacity:0.8;"),
          opts_tooltip(
            css = "background:white;padding:10px 14px;border-radius:6px;box-shadow:0 2px 8px rgba(0,0,0,0.15);font-size:12px;",
            use_fill = FALSE
          ),
          opts_sizing(rescale = TRUE)
        )
      )
    })
    
    # Feature descriptions table
    output$feature_table <- renderReactable({
      
      feature_desc <- data.frame(
        Feature = c(
          "Registration to Start",
          "Number of Sites",
          "Enrollment Size",
          "Enrollment per Site",
          "Number of Countries",
          "Trial Phase",
          "Sponsor Type"
        ),
        Description = c(
          "Days from ClinicalTrials.gov registration to first patient enrolled",
          "Total investigator sites participating (log-transformed)",
          "Actual (final) enrollment as reported at completion (log-transformed)",
          "Average patients per site = enrollment / sites (log-transformed)",
          "Number of countries with participating sites",
          "Phase 2, Phase 1/2, Phase 2/3, or Phase 3",
          "Industry, Academic/Other, NIH, or Government/Network"
        ),
        `Why It Matters` = c(
          "Captures operational readiness and startup complexity",
          "More sites = more coordination but potentially faster enrollment",
          "Larger trials take longer but with diminishing marginal effect",
          "High burden per site can slow enrollment and data collection",
          "International trials add regulatory and logistical complexity",
          "Later phases typically require more patients and longer follow-up",
          "Different sponsors have different operational capabilities"
        ),
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
      
      reactable(
        feature_desc,
        columns = list(
          Feature = colDef(
            width = 140,
            style = list(fontWeight = 600)
          ),
          Description = colDef(minWidth = 200),
          `Why It Matters` = colDef(minWidth = 200)
        ),
        bordered = FALSE,
        striped = TRUE,
        highlight = TRUE,
        compact = TRUE,
        style = list(fontSize = "0.9em")
      )
    })
    
    # Distribution chart
    output$distribution_chart <- renderGirafe({
      
      req(input$selected_feature)
      
      feature_col <- input$selected_feature
      
      # Get data
      plot_data <- trials_data |>
        select(value = all_of(feature_col)) |>
        filter(!is.na(value))
      
      # Create histogram
      p <- ggplot(plot_data, aes(x = value)) +
        geom_histogram_interactive(
          aes(
            tooltip = after_stat(paste0("Range: ", round(xmin, 0), " - ", round(xmax, 0), "\nCount: ", count))
          ),
          fill = colors$primary,
          color = "white",
          bins = 30
        ) +
        geom_vline(
          xintercept = median(plot_data$value, na.rm = TRUE),
          color = colors$warning,
          linetype = "dashed",
          linewidth = 1
        ) +
        annotate(
          "text",
          x = median(plot_data$value, na.rm = TRUE),
          y = Inf,
          label = paste0("Median: ", round(median(plot_data$value, na.rm = TRUE), 0)),
          vjust = 2,
          hjust = -0.1,
          color = colors$warning,
          fontface = "bold",
          size = 3.5
        ) +
        scale_x_continuous(labels = comma_format()) +
        scale_y_continuous(labels = comma_format()) +
        labs(
          x = unname(c(
            "duration_months" = "Trial Duration (months)",
            "enrollment_count" = "Enrollment (actual)",
            "n_sites" = "Number of Sites",
            "n_countries" = "Number of Countries",
            "registration_to_start" = "Registration to Start (days)"
          )[input$selected_feature]),
          y = "Number of Trials"
        ) +
        theme_minimal(base_size = 11) +
        theme(
          panel.grid.minor = element_blank()
        )
      
      girafe(
        ggobj = p,
        width_svg = 6,
        height_svg = 3.5,
        options = list(
          opts_hover(css = "fill:#023E8A;"),
          opts_tooltip(css = "background:white;padding:8px;border-radius:4px;box-shadow:0 2px 6px rgba(0,0,0,0.1);"),
          opts_sizing(rescale = TRUE)
        )
      )
    })
    
    # Distribution statistics
    output$distribution_stats <- renderUI({
      
      req(input$selected_feature)
      
      values <- trials_data[[input$selected_feature]]
      values <- values[!is.na(values)]
      
      unit <- switch(input$selected_feature,
                     "duration_months" = " months",
                     "enrollment_count" = " patients",
                     "n_sites" = " sites",
                     "n_countries" = " countries",
                     "registration_to_start" = " days",
                     ""
      )
      
      div(
        style = paste0("margin-top: 1em; padding: 1em; background: ", colors$background, "; border-radius: 8px;"),
        div(
          class = "ui four column grid",
          style = "text-align: center;",
          
          div(
            class = "column",
            div(style = "font-weight: 600;", round(min(values), 0)),
            div(style = paste0("font-size: 0.8em; color: ", colors$text_muted), "Min")
          ),
          div(
            class = "column",
            div(style = "font-weight: 600;", round(median(values), 0)),
            div(style = paste0("font-size: 0.8em; color: ", colors$text_muted), "Median")
          ),
          div(
            class = "column",
            div(style = "font-weight: 600;", round(mean(values), 0)),
            div(style = paste0("font-size: 0.8em; color: ", colors$text_muted), "Mean")
          ),
          div(
            class = "column",
            div(style = "font-weight: 600;", round(max(values), 0)),
            div(style = paste0("font-size: 0.8em; color: ", colors$text_muted), "Max")
          )
        )
      )
    })
    
  })
}