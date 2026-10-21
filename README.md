# Clinical Trial Duration Forecaster

*A portfolio case study in validation-first analytics using ClinicalTrials.gov oncology trials*

![R](https://img.shields.io/badge/R-276DC3?style=for-the-badge&logo=r&logoColor=white) ![Shiny](https://img.shields.io/badge/Shiny-1A162D?style=for-the-badge&logo=rstudio&logoColor=white) ![Status](https://img.shields.io/badge/Status-Active-2ECC71?style=for-the-badge) ![License](https://img.shields.io/badge/License-MIT-A8E10C?style=for-the-badge) ![Appsilon](https://img.shields.io/badge/Framework-Appsilon-0B6E6E?style=for-the-badge)

------------------------------------------------------------------------

## Overview

This project demonstrates **end-to-end predictive analytics** for clinical trial duration modeling—and why **validation strategy matters more than model selection**.

Holding the trials, features and model fixed, a random train/test split explains about a quarter of the variation in trial duration (**R² ≈ 0.24**). Training on trials started by 2015 and testing on trials started 2016–2018, which is how a forecast would actually be used, explains almost none of it (**R² ≈ 0.03**). About half of that drop (40–80% across five random splits) comes from the test window itself, since only trials that had finished by the data pull are included; the rest comes from predicting later trials.

**The finding is the insight:** registry fields carry only a weak signal about trial duration, weaker still for future trials, and only a validation that matches the intended use shows how weak.

📝 **Case study write-up:** [Clinical Trial Duration Forecaster: A Validation-First Case Study](https://stevenponce.netlify.app/projects/standalone_visualizations/sa_2026-01-19.html)

> **Correction (October 2026).** Earlier versions of this project reported a random-split R² of **0.84** and a time-based R² of **0.04**, and attributed the gap to temporal drift. Both figures were wrong. `lubridate::ymd()` misread ClinicalTrials.gov's month-only dates (e.g. `"2005-08"` was parsed as 2020-05-08), which mis-dated 609 trials, dropped 975 others, and produced the spurious 0.84. A follow-up check also found 74 Phase 2/3 trials counted twice (returned by both the Phase 2 and Phase 3 queries). The pipeline now parses month-only dates as the first of the month and removes duplicates; the data were rebuilt, the models retrained, and every number in the app is computed from the saved artifacts. The counts are reproduced by `scripts/07_date_parsing_audit.R`. The conclusion that registry data cannot forecast trial duration still holds; the size of the random-split result does not.

------------------------------------------------------------------------

## Live Dashboard

## 🔗 **Interactive dashboard (ShinyApps.io):** [Clinical Trial Duration Forecaster](https://0l6jpd-steven-ponce.shinyapps.io/clinical-trial-forecaster/)

## Key Finding

Same cohort (2,457 trials started 2018 or earlier), recipe, tuning grid and Elastic Net model; only the split changes (`scripts/06_random_vs_time_split.R`).

| Validation Approach | Test R² (1 − SSE/SST) | Squared correlation | Test Median Absolute Error | Interpretation |
|----|---:|---:|---:|----|
| Random split (mean of 5 seeds) | 0.24 | 0.24 | 20.6 months | Modest: describes historical patterns |
| Random split, scored on trials ≤ 112 months | 0.13 | 0.16 | 17.6 months | Same truncated window as the time-based test |
| **Time-based split** | **0.03** | **0.11** | **15.2 months** | **Very little prospective signal** |

### Why the gap?

-   **Registry limitations:** ClinicalTrials.gov lacks protocol complexity, endpoint strategy, and competitive dynamics, so the signal that exists is weak and partly era-specific
-   **Right-truncation of the test window:** only completed trials are included, so a 2016–2018 trial had to finish by the January 2026 data pull (longest: 112 months). Long trials are missing, which shifts test durations down (test trials ran on average about 13% shorter than predicted) and slightly narrows their range. Both lower R², and both make errors in months look *smaller* than on the random split. Scoring the random split on the same window (middle row) approximately separates this effect from the effect of time: roughly half the drop (40–80% across splits) is the window. That is probably a lower bound, because the check applies a single 112-month cap while 2018 starts that finished ran at most 89 months
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
Training:  Trials with start date ≤2015 (n=2,079)
Test:      Trials with start date 2016–2018 (n=378)
Excluded:  Trials with start date ≥2019 (n=253, right-truncation bias)
```

Trials starting in 2019+ were excluded because only shorter-duration studies from that era are fully completed and present in the dataset as of the January 2026 data pull. Including them would bias observed durations downward.

### Model Comparison (CV within training data)

10-fold CV within the training data; each row's metrics come from the same configuration (best CV RMSE). CV R² here is yardstick's squared correlation.

| Model             | CV R² (sq. corr.) | CV RMSE (log-months) | ± 1 SE |
|-------------------|------:|---------------------:|-------:|
| **Elastic Net**   | **0.267** |        **0.557** |  0.011 |
| Linear Regression | 0.266 |                0.557 |  0.011 |
| Random Forest     | 0.263 |                0.560 |  0.009 |
| XGBoost           | 0.256 |                0.561 |  0.010 |

All four are within one standard error of the best on RMSE, so they are effectively tied. **Final model used in the dashboard:** Elastic Net (glmnet), kept by a stated preference for a regularized linear model with readable coefficients; at the small penalty CV chose (0.001) it is nearly identical to plain linear regression. Its time-based test performance: R² 0.03 (squared correlation 0.11), RMSE 0.604 (log scale), median absolute error 15.2 months.

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
-   The January 2026 pull was capped at 2,000 trials per phase query (Phase 2 and Phase 3, in API order), so the cohort is a large sample of completed oncology trials, not all of them The API's return order shows no time trend within either pull (correlation of position with start year 0.04 and −0.03; median start year 2009 in the first and last quarter of each), so the cap does not appear to cut by date (`scripts/09_sampling_order_check.R`). The full registry population was not re-counted.

------------------------------------------------------------------------

## Key Design Principles

-   **Validation-first thinking** — Match strategy to use case
-   **Transparency over performance** — Report honest metrics
-   **Explainability over complexity** — Linear model over black box
-   **Appropriate restraint** — Document what the tool cannot do
-   **Decision support, not automation** — Inform, don't prescribe

------------------------------------------------------------------------

## Limitations

-   Oncology cohort defined via keyword-based registry fields (imperfect). About 8% of modeled trials (203 of 2,457) list no cancer term in their conditions field. Refitting without them changes test R² from 0.030 to 0.018 and median absolute error from 15.2 to 14.9 months; conclusions are unchanged (`scripts/05_sensitivity_oncology_filter.R`).
-   Registry fields are self-reported and updated over time
-   Inputs (actual enrollment, sites, countries) are final values, known only once a trial is under way or finished. With only what is known at study start (phase, sponsor, registration-to-start lag), R² falls from 0.24 to 0.10 on a random split and from 0.03 to −0.07 on later trials (`scripts/08_prestart_features.R`): most of the signal comes from hindsight inputs
-   Limited prospective predictive power (time-based R² ≈ 0.03)
-   Each Phase 2/3 trial is counted once; the API pull was split by phase, and duplicates across queries are removed
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
│   │   ├── sensitivity_oncology_filter.csv # cohort sensitivity check (script 05)
│   │   ├── date_parsing_audit.csv          # counts behind the correction note (script 07)
│   │   ├── prestart_comparison.csv         # pre-start-only features (script 08)
│   │   └── sampling_order_check.csv        # API return-order check (script 09)
│   ├── data-raw/
│   │   ├── modeling/            # Feature table read by the app
│   │   └── cache/               # Jan 2026 ClinicalTrials.gov snapshot (not deployed)
│   ├── docs/                    # Diagnostic plots (not deployed)
│   └── www/
│       └── styles.css
├── scripts/                     # Data pipeline
│   ├── 00_setup.R
│   ├── run_all.R                # rebuilds every app artifact (02-09)
│   ├── 01_api_connection_test.R
│   ├── 02_download_and_clean.R
│   ├── 03_feature_engineering.R
│   ├── 04_modeling.R
│   ├── 05_sensitivity_oncology_filter.R
│   ├── 06_random_vs_time_split.R
│   ├── 07_date_parsing_audit.R
│   ├── 08_prestart_features.R
│   └── 09_sampling_order_check.R
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

**Steven Ponce** Senior Manager, Clinical Analytics & Centralized Monitoring \| R & Shiny

🔗 **Portfolio Website**: [stevenponce.netlify.app](https://stevenponce.netlify.app/projects.html) 🐙 **GitHub**: [\@poncest](https://github.com/poncest) 💼 **LinkedIn**: [stevenponce](https://www.linkedin.com/in/stevenponce/) 🦋 **Bluesky**: [\@sponce1](https://bsky.app/profile/sponce1.bsky.social)

------------------------------------------------------------------------

*Prepared by Steven Ponce as part of a professional analytics portfolio.* *Demonstrating that honest validation findings—even when they reveal limitations—are more valuable than inflated metrics.*
