# Clinical Trial Forecaster

**R Shiny dashboard for predicting clinical trial enrollment timelines and identifying at-risk trials using ClinicalTrials.gov data.**

[![Status](https://img.shields.io/badge/status-in%20development-yellow)]()
[![R](https://img.shields.io/badge/R-4.3%2B-blue)]()
[![License](https://img.shields.io/badge/license-MIT-green)]()

---

## Overview

This dashboard demonstrates predictive analytics capabilities for clinical trial planning and risk assessment. Using publicly available data from ClinicalTrials.gov, it forecasts enrollment timelines, identifies geographic risk factors, and models phase transition probabilities.

**Key Features:**
- Enrollment timeline prediction using machine learning
- Geographic risk scoring by country/region
- Phase transition probability modeling
- Therapeutic area focus: Oncology/Hematology

---

## Strategic Questions

This project addresses key analytical questions relevant to clinical operations:

1. **Enrollment Forecasting:** Can we predict how long a trial will take to enroll based on its characteristics?
2. **Risk Identification:** Which trials are most likely to miss their enrollment targets?
3. **Geographic Patterns:** How do enrollment rates vary by country and region?
4. **Phase Transitions:** What are realistic timelines for advancing through clinical phases?

---

## Dashboard Tabs

| Tab | Purpose |
|-----|---------|
| **Executive Brief** | Model performance summary, key predictions, risk alerts |
| **Enrollment Forecaster** | Interactive prediction tool with scenario analysis |
| **Geographic Risk** | Country-level enrollment patterns and risk scoring |
| **Phase Transitions** | Historical success rates and timeline distributions |
| **Methods & Data** | Model documentation, data sources, validation results |

---

## Data Source

**ClinicalTrials.gov** - U.S. National Library of Medicine
- Trial registration details (phase, therapeutic area, conditions)
- Enrollment targets and actual enrollment
- Timeline data (start dates, completion dates)
- Geographic information (countries, facilities)

---

## Technology Stack

- **Framework:** R Shiny with shiny.semantic
- **Visualization:** ggplot2, ggiraph, reactable
- **Modeling:** tidymodels ecosystem
- **Data:** ClinicalTrials.gov API via `ctrialsgov` package

---
## Project Status

🚧 **In Development** - January 2026

### Roadmap

- [ ] Phase 1: Data acquisition and exploration
- [ ] Phase 2: Feature engineering and baseline models
- [ ] Phase 3: Dashboard MVP (Executive Brief + Forecaster)
- [ ] Phase 4: Geographic and phase transition modules
- [ ] Phase 5: Model refinement and validation
- [ ] Phase 6: Documentation and deployment

---

## Author

**Steven Ponce**  
Pharmaceutical Data Scientist | Risk Analytics

- [Portfolio](https://stevenponce.netlify.app/)
- [GitHub](https://github.com/poncest)
- [LinkedIn](https://www.linkedin.com/in/stevenponce/)

---

## License

MIT License - See [LICENSE](LICENSE) for details.

---

*This project demonstrates predictive analytics capabilities for clinical trial planning using public data.*
