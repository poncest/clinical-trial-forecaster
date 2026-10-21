# Clinical Trial Duration Forecaster

*A portfolio case study in validation-first analytics using ClinicalTrials.gov oncology trials*

![R](https://img.shields.io/badge/R-276DC3?style=for-the-badge&logo=r&logoColor=white) ![Shiny](https://img.shields.io/badge/Shiny-1A162D?style=for-the-badge&logo=rstudio&logoColor=white) ![Status](https://img.shields.io/badge/Status-Active-2ECC71?style=for-the-badge) ![License](https://img.shields.io/badge/License-MIT-A8E10C?style=for-the-badge) ![Appsilon](https://img.shields.io/badge/Framework-Appsilon-0B6E6E?style=for-the-badge)

------------------------------------------------------------------------

## Overview

This project demonstrates **end-to-end predictive analytics** for clinical trial duration modeling—and why **validation strategy matters more than model selection**.

Holding the trials, features and model fixed, a random train/test split explains about a quarter of the variation in trial duration (**R² ≈ 0.23**). Training on trials started by 2015 and testing on trials started 2016–2018, which is how a forecast would actually be used, explains almost none of it (**R² ≈ 0.02**).

**The finding is the insight:** registry fields describe some historical pattern but carry almost no prospective signal, and only a validation that matches the intended use shows it.

📝 **Case study write-up:** [Clinical Trial Duration Forecaster: A Validation-First Case Study](https://stevenponce.netlify.app/projects/standalone_visualizations/sa_2026-01-19.html)

> **Correction (October 2026).** Earlier versions of this project reported a random-split R² of **0.84** and a time-based R² of **0.04**, and attributed the gap to temporal drift. Both figures were wrong. `lubridate::ymd()` misread ClinicalTrials.gov's month-only dates (e.g. `"2005-08"` was parsed as 2020-05-08), which mis-dated 622 trials, dropped about 1,000 others, and produced the spurious 0.84. The pipeline now parses month-only dates as the first of the month; the data were rebuilt, the models retrained, and every number in the app is computed from the saved artifacts. The conclusion that registry data cannot forecast trial duration still holds; the size of the random-split result does not.

------------------------------------------------------------------------

## Live Dashboard

## 🔗 **Interactive dashboard (ShinyApps.io):** [Clinical Trial Duration Forecaster](https://0l6jpd-steven-ponce.shinyapps.io/clinical-trial-forecaster/)

## Key Finding

Same cohort (2,499 trials started 2018 or earlier), recipe, tuning grid and Elastic Net model; only the split changes (`scripts/06_random_vs_time_split.R`).

| Validation Approach | Test R² (1 − SSE/SST) | Squared correlation | Test Median Absolute Error | Interpretation |
|----|---:|---:|---:|----|
| Random split (mean of 5 seeds) | 0.23 | 0.23 | 20.4 months | Modest: describes historical patterns |
| **Time-based split** | **0.02** | **0.11** | **15.2 months** | **Almost no prospective signal** |

### Why the gap?

-   **Registry limitations:** ClinicalTrials.gov lacks protocol complexity, endpoint strategy, and competitive dynamics, so the signal that exists is weak and partly era-specific
-   **Right-truncation of the test window:** only completed trials are included, so a 2016–2018 trial had to finish by the January 2026 data pull (longest: 112 months). That squeezes the test durations into a narrower range, which lowers R² and makes errors in months look *smaller* than on the random split. R², not error in months, is the fair comparison
-   **Later trials look shorter, partly by construction:** median duration is 64 months for trials started by 2015 and 52 months for 2016–2018; among earlier trials no longer than 112 months the median is 56. Most of the apparent shift is the truncation above, not a change in practice

This is not "model failure"—it shows why validation must match the intended decision context.

------------------------------------------------------------------------

## Dashboard Preview

### Executive Brief

![Executive Brief](screenshots/executive_brief.png) *Key metrics (time-based R² vs random split), sponsor and size findings, and model summary*

### Duration Forecaster (Scenario Sandbox)

![Duration Forecaster](screenshots/forecaster.png) *Interactive tool showing how the model behaves, with an empirical likely range from test-set errors—not for operational planning*

### Feature Explorer

![Feature Explorer](screenshots/explorer.png) *Coefficient analysis and feature distributions*

### Model Performance

![Model Performance](screenshots/performance.png) *Diagnostic plots with actual error analysis by phase and sponsor*

### Methods & Data

![Methods & Data](screenshots/methods.png) *Validation Lessons panel explaining why metrics can mislead*

------------------------------------------------------------------------

## Purpose (and Boundaries)

The dashboard helps answer:

-   How do registry-level trial characteristics relate to historical duration?
-   What are the limits of registry-based forecasting in oncology?
-   How does validation choice change conclusions?
-   Where does the model fail, and why?

**This tool is intended for methodology and pattern understanding—not operational planning.**

------------------------------------------------------------------------

## What This Dashboard Does *Not* Do

To avoid overreach, this project does **not**:

-   Provide reliable prospective duration forecasts for planning
-   Recommend protocol or design decisions
-   Predict clinical success, approvals, or outcomes
-   Substitute for protocol-level feasibility analysis

All results are conditional on historical registry patterns that may not persist.

------------------------------------------------------------------------

## Analytical Framework

### Data Pipeline

1.  **Acquisition:** ClinicalTrials.gov API v2 (pulled January 2026)
2.  **Cohort:** Interventional, treatment-purpose oncology trials (keyword-based condition query; imperfect). Month-only registry dates are parsed as the first of the month
3.  **Outcome:** Duration in months from **Start Date → Completion Date** (includes follow-up; not equivalent to LPLV)
4.  **Features:** Enrollment, sites, countries, sponsor type, phase, registration-to-start lag, derived ratios/log transforms (final reported values, known only in hindsight)
5.  **Model:** **Elastic Net (glmnet)** via tidymodels (penalty 0.001, mixture 0.5)

### Validation Strategy (Time-Based)

```         
Training:  Trials with start date ≤2015 (n=2,112)
Test:      Trials with start date 2016–2018 (n=387)
Excluded:  Trials with start date ≥2019 (n=256, right-truncation bias)
```

Trials starting in 2019+ were excluded because only shorter-duration studies from that era are fully completed and present in the dataset as of the January 2026 data pull. Including them would bias observed durations downward.

### Model Comparison (CV within training data)

10-fold CV within the training data; each row's metrics come from the same configuration (best CV RMSE).

| Model             | CV R² | CV RMSE (log-months) | ± 1 SE |
|-------------------|------:|---------------------:|-------:|
| Random Forest     | 0.276 |                0.556 |  0.010 |
| **Elastic Net**   | **0.265** |        **0.559** |  0.007 |
| Linear Regression | 0.264 |                0.560 |  0.007 |
| XGBoost           | 0.257 |                0.562 |  0.009 |

All four are within one standard error of the best on RMSE, so they are effectively tied. **Final model used in the dashboard:** Elastic Net (glmnet), the simplest interpretable model in that tie. Its time-based test performance: R² 0.02, RMSE 0.608 (log scale), median absolute error 15.2 months.

------------------------------------------------------------------------

## Data Sources

-   **ClinicalTrials.gov** (U.S. National Library of Medicine)
-   **API v2**, accessed January 2026

### Inclusion Criteria

-   Interventional studies
-   Primary purpose: treatment
-   Phase 1/2, 2, 2/3, or 3
-   Completed with valid start/completion dates
-   Actual enrollment (final posted value), 5–10,000 participants
-   Duration 1–180 months

### Data Quality Notes

-   Registry fields are self-reported and updated over time
-   Historical snapshots are not available in this build
-   Oncology cohort defined via keyword-based registry fields (imperfect)
-   The January 2026 pull was capped at 2,000 trials per phase query (Phase 2 and Phase 3, in API order), so the cohort is a large sample of completed oncology trials, not all of them

------------------------------------------------------------------------

## Key Design Principles

-   **Validation-first thinking** — Match strategy to use case
-   **Transparency over performance** — Report honest metrics
-   **Explainability over complexity** — Linear model over black box
-   **Appropriate restraint** — Document what the tool cannot do
-   **Decision support, not automation** — Inform, don't prescribe

------------------------------------------------------------------------

## Limitations

-   Oncology cohort defined via keyword-based registry fields (imperfect). About 8% of modeled trials (208 of 2,499) list no cancer term in their conditions field. Refitting without them changes test R² from 0.023 to 0.019 and median absolute error from 15.2 to 14.7 months; conclusions are unchanged (`scripts/05_sensitivity_oncology_filter.R`).
-   Registry fields are self-reported and updated over time
-   Inputs (actual enrollment, sites, countries) are final values, known only once a trial is under way or finished
-   Limited prospective predictive power (time-based R² ≈ 0.02)
-   Survivorship and right-truncation bias (completed trials only; long recent trials are missing)

------------------------------------------------------------------------

## Repository Structure

```         
clinical-trial-forecaster/
├── app/
│   ├── app.R                    # Entry point
│   ├── global.R                 # Data loading, metrics, helpers
│   ├── ui.R                     # Main UI definition
│   ├── DESCRIPTION              # Package dependencies for shinyapps.io
│   ├── modules/
│   │   ├── mod_executive_brief.R
│   │   ├── mod_forecaster.R
│   │   ├── mod_features.R
│   │   ├── mod_performance.R
│   │   └── mod_methods.R
│   ├── models/                  # Trained model artifacts
│   │   ├── final_workflow.rds
│   │   ├── feature_importance.rds
│   │   ├── model_comparison.csv
│   │   ├── test_predictions.csv
│   │   ├── split_comparison.csv            # random vs time-based split (script 06)
│   │   └── sensitivity_oncology_filter.csv # cohort sensitivity check (script 05)
│   ├── data-raw/
│   │   ├── modeling/            # Feature table read by the app
│   │   └── cache/               # Jan 2026 ClinicalTrials.gov snapshot (not deployed)
│   ├── docs/                    # Diagnostic plots (not deployed)
│   └── www/
│       └── styles.css
├── scripts/                     # Data pipeline
│   ├── 00_setup.R
│   ├── 01_data_acquisition.R
│   ├── 02_data_exploration.R
│   ├── 03_feature_engineering.R
│   ├── 04_modeling.R
│   ├── 05_sensitivity_oncology_filter.R
│   └── 06_random_vs_time_split.R
└── screenshots/
```

------------------------------------------------------------------------

## Technical Stack

-   **Language:** R
-   **Framework:** Shiny (modular architecture)
-   **UI Components:** shiny.semantic (Appsilon)
-   **Visualization:** ggplot2, ggiraph, reactable
-   **ML Framework:** tidymodels
-   **Deployment:** ShinyApps.io

------------------------------------------------------------------------

## License

This project is released under the **MIT License**.

------------------------------------------------------------------------

## Disclaimer

This project is for **educational and portfolio purposes only**.

It does not constitute clinical, operational, or planning advice. All parameters are derived from publicly available registry data. No representation is made regarding applicability to specific trial contexts.

------------------------------------------------------------------------

## Contact

**Steven Ponce** Data Analyst \| R Shiny Developer \| Business Intelligence Specialist

🔗 **Portfolio Website**: [stevenponce.netlify.app](https://stevenponce.netlify.app/projects.html) 🐙 **GitHub**: [\@poncest](https://github.com/poncest) 💼 **LinkedIn**: [stevenponce](https://www.linkedin.com/in/stevenponce/) 🦋 **Bluesky**: [\@sponce1](https://bsky.app/profile/sponce1.bsky.social)

------------------------------------------------------------------------

*Prepared by Steven Ponce as part of a professional analytics portfolio.* *Demonstrating that honest validation findings—even when they reveal limitations—are more valuable than inflated metrics.*
