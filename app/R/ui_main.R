# =============================================================================
# Clinical Trial Duration Forecaster - Main UI
# =============================================================================

ui <- semanticPage(
  title = "Clinical Trial Duration Forecaster",
  
  # Custom CSS
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
    tags$style(HTML(paste0("
      /* Global styles */
      body {
        background-color: ", colors$background, ";
        font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, sans-serif;
      }
      
      /* Sidebar styling */
      .sidebar-container {
        background: linear-gradient(180deg, ", colors$primary_dark, " 0%, ", colors$primary, " 100%);
        min-height: 100vh;
        padding: 1.5em;
        color: white;
      }
      
      .sidebar-title {
        font-size: 1.4em;
        font-weight: 600;
        margin-bottom: 0.3em;
        color: white;
      }
      
      .sidebar-subtitle {
        font-size: 0.9em;
        opacity: 0.85;
        margin-bottom: 2em;
      }
      
      /* Menu items */
      .sidebar-menu .item {
        color: rgba(255,255,255,0.9) !important;
        padding: 0.9em 1em !important;
        border-radius: 8px !important;
        margin-bottom: 0.3em !important;
        transition: all 0.2s ease !important;
      }
      
      .sidebar-menu .item:hover {
        background: rgba(255,255,255,0.15) !important;
        color: white !important;
      }
      
      .sidebar-menu .item.active {
        background: rgba(255,255,255,0.25) !important;
        color: white !important;
        font-weight: 500;
      }
      
      .sidebar-menu .item i.icon {
        margin-right: 0.7em !important;
      }
      
      /* Main content area */
      .main-content {
        padding: 2em;
        background-color: ", colors$background, ";
        min-height: 100vh;
      }
      
      /* Card styling */
      .ui.segment {
        border-radius: 12px !important;
        box-shadow: 0 1px 3px rgba(0,0,0,0.08) !important;
        border: 1px solid ", colors$border, " !important;
      }
      
      /* KPI cards */
      .kpi-card {
        text-align: center;
        padding: 1.5em;
        background: white;
        border-radius: 12px;
        box-shadow: 0 1px 3px rgba(0,0,0,0.08);
        border: 1px solid ", colors$border, ";
      }
      
      .kpi-value {
        font-size: 2.5em;
        font-weight: 700;
        color: ", colors$primary, ";
        line-height: 1.2;
      }
      
      .kpi-label {
        font-size: 0.95em;
        color: ", colors$text_secondary, ";
        margin-top: 0.3em;
      }
      
      .kpi-context {
        font-size: 0.85em;
        color: ", colors$text_muted, ";
        margin-top: 0.5em;
      }
      
      /* Hypothesis/insight boxes */
      .insight-box {
        padding: 1.2em 1.5em;
        border-left: 4px solid ", colors$primary, ";
        background: ", colors$background, ";
        border-radius: 0 8px 8px 0;
        margin: 1em 0;
      }
      
      .insight-title {
        font-weight: 600;
        color: ", colors$primary_dark, ";
        margin-bottom: 0.5em;
      }
      
      /* Tab content headers */
      .tab-header {
        margin-bottom: 1.5em;
      }
      
      .tab-title {
        font-size: 1.5em;
        font-weight: 600;
        color: ", colors$text_primary, ";
        margin-bottom: 0.3em;
      }
      
      .tab-subtitle {
        color: ", colors$text_secondary, ";
        font-size: 1em;
      }
      
      /* Prediction result styling */
      .prediction-result {
        text-align: center;
        padding: 2em;
        background: linear-gradient(135deg, ", colors$primary, " 0%, ", colors$primary_dark, " 100%);
        border-radius: 12px;
        color: white;
      }
      
      .prediction-value {
        font-size: 3.5em;
        font-weight: 700;
        line-height: 1.1;
      }
      
      .prediction-label {
        font-size: 1.1em;
        opacity: 0.9;
        margin-top: 0.5em;
      }
      
      /* Footer */
      .app-footer {
        background: ", colors$text_primary, ";
        color: rgba(255,255,255,0.8);
        padding: 1.5em;
        text-align: center;
        font-size: 0.9em;
        margin-top: 2em;
      }
      
      .app-footer a {
        color: ", colors$primary_light, ";
      }
      
      /* reactable styling */
      .rt-table {
        font-size: 0.9em;
      }
      
      /* ggiraph tooltip */
      .tooltip_ggiraph {
        background: white;
        padding: 8px 12px;
        border-radius: 6px;
        box-shadow: 0 2px 8px rgba(0,0,0,0.15);
        font-size: 0.9em;
      }
    ")))
  ),
  
  # Main layout: Sidebar + Content
  div(
    class = "ui grid",
    style = "margin: 0; min-height: 100vh;",
    
    # Sidebar (3 columns wide)
    div(
      class = "three wide column sidebar-container",
      
      # Logo/Title
      div(
        class = "sidebar-title",
        icon("chart line"),
        " Trial Forecaster"
      ),
      div(
        class = "sidebar-subtitle",
        "Duration Prediction Tool"
      ),
      
      # Navigation Menu
      div(
        class = "ui vertical fluid menu sidebar-menu",
        style = "background: transparent; border: none; box-shadow: none;",
        
        a(
          class = "item active",
          `data-tab` = "exec_brief",
          icon("dashboard"), "Executive Brief"
        ),
        a(
          class = "item",
          `data-tab` = "forecaster",
          icon("calculator"), "Duration Forecaster"
        ),
        a(
          class = "item",
          `data-tab` = "features",
          icon("chart bar"), "Feature Explorer"
        ),
        a(
          class = "item",
          `data-tab` = "performance",
          icon("check circle"), "Model Performance"
        ),
        a(
          class = "item",
          `data-tab` = "methods",
          icon("book"), "Methods & Data"
        )
      ),
      
      # Sidebar footer
      div(
        style = "position: absolute; bottom: 1.5em; left: 1.5em; right: 1.5em;",
        
        div(
          style = "border-top: 1px solid rgba(255,255,255,0.2); padding-top: 1em; margin-bottom: 1em;"
        ),
        
        # About button - uses semantic UI modal
        tags$button(
          class = "ui inverted basic button fluid",
          style = "margin-bottom: 0.8em;",
          onclick = "$('.ui.modal.about-modal').modal('show');",
          icon("info circle"), " About"
        ),
        
        # Data info
        div(
          style = "font-size: 0.8em; opacity: 0.7; text-align: center;",
          paste0(format(app_info$n_trials, big.mark = ","), " oncology trials"),
          br(),
          "ClinicalTrials.gov",
          br(),
          paste0("Model: ", app_info$model_trained)
        )
      )
    ),
    
    # About Modal (semantic UI modal - placed before main content)
    div(
      class = "ui modal about-modal",
      div(
        class = "header",
        icon("info circle"), " About This Dashboard"
      ),
      div(
        class = "content",
        style = "line-height: 1.7;",
        
        h4(style = paste0("color: ", colors$primary_dark, "; margin-top: 0;"), "Purpose"),
        p("This dashboard demonstrates predictive analytics capabilities for clinical trial 
           planning. It uses machine learning to forecast trial duration based on trial 
           characteristics, providing decision support for portfolio planning and resource 
           allocation."),
        
        h4(style = paste0("color: ", colors$primary_dark), "Data Source"),
        p(HTML(paste0(
          "Analysis based on <strong>", format(app_info$n_trials, big.mark = ","), 
          " completed oncology trials</strong> from ",
          "<a href='https://clinicaltrials.gov' target='_blank'>ClinicalTrials.gov</a>. ",
          "Includes Phase 2, Phase 2/3, and Phase 3 interventional trials with complete ",
          "duration data."
        ))),
        
        h4(style = paste0("color: ", colors$primary_dark), "Model"),
        tags$ul(
          tags$li(HTML("<strong>Algorithm:</strong> Random Forest (500 trees)")),
          tags$li(HTML(paste0("<strong>Performance:</strong> R² = ", model_metrics$r_squared, 
                              " on held-out test data"))),
          tags$li(HTML(paste0("<strong>Median Error:</strong> ", model_metrics$median_abs_error_months, 
                              " months")))
        ),
        
        h4(style = paste0("color: ", colors$primary_dark), "Disclaimer"),
        p(style = "color: #666; font-style: italic;",
          "This is a portfolio demonstration project. Predictions are based on historical 
           patterns and should not be used for actual trial planning without validation 
           against company-specific data and expert review."),
        
        hr(),
        
        h4(style = paste0("color: ", colors$primary_dark), "Technology Stack"),
        p("R Shiny • shiny.semantic • tidymodels • ggiraph • reactable"),
        
        div(
          style = "margin-top: 1.5em; display: flex; gap: 1em;",
          tags$a(
            href = "https://github.com/poncest/clinical-trial-forecaster",
            target = "_blank",
            class = "ui button",
            icon("github"), " View on GitHub"
          ),
          tags$a(
            href = "https://www.linkedin.com/in/stevenponce/",
            target = "_blank",
            class = "ui button",
            icon("linkedin"), " LinkedIn"
          )
        )
      ),
      div(
        class = "actions",
        div(
          class = "ui approve button",
          "Close"
        )
      )
    ),
    
    # Main Content (13 columns wide)
    div(
      class = "thirteen wide column main-content",
      
      # Tab content containers
      div(
        class = "ui tab active",
        `data-tab` = "exec_brief",
        mod_executive_brief_ui("exec_brief")
      ),
      
      div(
        class = "ui tab",
        `data-tab` = "forecaster",
        mod_forecaster_ui("forecaster")
      ),
      
      div(
        class = "ui tab",
        `data-tab` = "features",
        mod_features_ui("features")
      ),
      
      div(
        class = "ui tab",
        `data-tab` = "performance",
        mod_performance_ui("performance")
      ),
      
      div(
        class = "ui tab",
        `data-tab` = "methods",
        mod_methods_ui("methods")
      )
    )
  ),
  
  # JavaScript for tab switching
  tags$script(HTML("
    $(document).ready(function() {
      $('.sidebar-menu .item').on('click', function() {
        var tab = $(this).attr('data-tab');
        
        // Update menu active state
        $('.sidebar-menu .item').removeClass('active');
        $(this).addClass('active');
        
        // Update tab visibility
        $('.ui.tab').removeClass('active');
        $('.ui.tab[data-tab=\"' + tab + '\"]').addClass('active');
        
        // Force Shiny to bind inputs/outputs in newly visible tab (with delay)
        setTimeout(function() {
          Shiny.bindAll();
          $(window).trigger('resize');
        }, 100);
      });
    });
  "))
)