# Clinical Trial Duration Forecaster

[![R](https://img.shields.io/badge/R-4.3+-blue.svg)](https://www.r-project.org/)
[![Shiny](https://img.shields.io/badge/Shiny-Dashboard-brightgreen.svg)](https://shiny.posit.co/)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

> Predictive analytics dashboard for clinical trial duration forecasting using machine learning and public registry data.

## 🎯 Overview

This dashboard demonstrates end-to-end predictive analytics capabilities by forecasting clinical trial duration based on trial characteristics. The model was trained on 2,057 completed oncology trials from ClinicalTrials.gov and achieves **R² = 0.79** on held-out test data.

### Strategic Questions Addressed

1. **How long will this trial take?** — Interactive prediction tool for planning
2. **What drives duration?** — Feature importance analysis reveals key predictors
3. **How reliable are estimates?** — Model diagnostics and validation metrics
4. **Where are the limits?** — Transparent methodology and limitations

## 📊 Model Performance

| Metric | Value |
|--------|-------|
| R² (Test Set) | 0.790 |
| RMSE (log scale) | 0.652 |
| Median Absolute Error | 9.6 months |
| Mean Absolute Error | 18.1 months |

### Top Predictors

1. **Registration-to-Start Time** — Days from registration to first patient (dominant predictor)
2. **Number of Sites** — Trial operational complexity
3. **Enrollment Size** — Target sample size
4. **Enrollment per Site** — Site burden ratio
5. **Number of Countries** — Geographic complexity

## 🖥️ Dashboard Features

### Executive Brief
![Executive Brief](screenshots/executive_brief.png)
*High-level model summary with key metrics and strategic insights*

### Duration Forecaster
![Duration Forecaster](screenshots/forecaster.png)
*Interactive prediction interface with contextual comparison*

### Feature Explorer
![Feature Explorer](screenshots/features.png)
*Feature importance visualization and distribution analysis*

### Model Performance
![Model Performance](screenshots/performance.png)
*Diagnostic plots, validation metrics, and model comparison*

### Methods & Data
*Comprehensive documentation of data sources, methodology, and limitations*

## 🚀 Getting Started

### Prerequisites

```r
# Required packages
install.packages(c(
  "shiny", "shiny.semantic", 
  "dplyr", "tidyr", "ggplot2", "scales",
  "ggiraph", "reactable", "reactablefmtr",
  "workflows", "parsnip", "recipes"
))
```

### Running Locally

```r
# Clone repository
# Navigate to app directory
setwd("clinical-trial-forecaster/app")

# Run app
shiny::runApp()
```

### Data Files Required

Place these files in `app/data/processed/`:
- `final_workflow.rds` — Trained Random Forest model
- `trials_features.rds` — Feature data for reference
- `feature_importance.rds` — Variable importance scores
- `model_comparison.csv` — Model comparison results
- `test_predictions.csv` — Test set predictions

## 📁 Project Structure

```
clinical-trial-forecaster/
├── app/
│   ├── app.R                    # Main entry point
│   ├── global.R                 # Load packages, data, helpers
│   ├── R/
│   │   └── ui_main.R            # Main UI layout
│   ├── modules/
│   │   ├── mod_executive_brief.R
│   │   ├── mod_forecaster.R
│   │   ├── mod_features.R
│   │   ├── mod_performance.R
│   │   └── mod_methods.R
│   ├── data/processed/          # Model and data files
│   └── www/
│       └── styles.css           # Custom styling
├── scripts/                     # Data pipeline scripts
├── models/                      # Saved model artifacts
├── docs/                        # Diagnostic plots
└── screenshots/                 # README images
```

## 🔬 Methodology

### Data Pipeline

1. **Data Acquisition** — ClinicalTrials.gov API (v2) for oncology trials
2. **Data Filtering** — Phase 2/3, completed, valid dates
3. **Feature Engineering** — Log transforms, ratios, categorical encoding
4. **Model Training** — 10-fold CV, hyperparameter tuning
5. **Evaluation** — Test set validation, diagnostics

### Model Selection

| Model | CV RMSE | CV R² |
|-------|---------|-------|
| **Random Forest** | **0.581** | **0.832** |
| XGBoost | 0.584 | 0.830 |
| Elastic Net | 0.678 | 0.771 |
| Linear Regression | 0.679 | 0.770 |

Random Forest selected for competitive performance with interpretability.

## ⚠️ Limitations

- **Oncology-specific** — Model trained only on oncology trials
- **Survivorship bias** — Only completed trials included
- **Registry data** — Lacks protocol-level detail
- **Historical patterns** — May not capture recent operational changes

## 📚 References

- ClinicalTrials.gov — [https://clinicaltrials.gov](https://clinicaltrials.gov)
- tidymodels — [https://tidymodels.org](https://tidymodels.org)
- shiny.semantic — [https://appsilon.github.io/shiny.semantic/](https://appsilon.github.io/shiny.semantic/)

## 👤 Author

**Steven Ponce**
- Portfolio: [stevenponce.com](https://stevenponce.com)
- LinkedIn: [linkedin.com/in/stevenponce](https://linkedin.com/in/stevenponce)
- GitHub: [github.com/poncest](https://github.com/poncest)

## 📄 License

This project is licensed under the MIT License.

---

*Built as a portfolio demonstration of predictive analytics and dashboard development capabilities.*
