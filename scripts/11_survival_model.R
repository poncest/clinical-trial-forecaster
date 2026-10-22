# ============================================================================
# 11_survival_model.R
# Clinical Trial Forecaster - Censoring-aware check
# ============================================================================
# Purpose: The published cohort keeps only COMPLETED trials, so long trials
#          that were still running at the data pull are missing. This uses the
#          full all-status pull (script 10) and treats unfinished trials as
#          censored:
#            - event: completed (completion date not marked as estimated)
#            - censored at the pull date: still running (active, recruiting,
#              enrolling by invitation, suspended)
#            - censored at the stop date: terminated (never completed)
#            - excluded: withdrawn / not yet recruiting (never started),
#              UNKNOWN status (dates not verified for 2+ years), and finished
#              trials with no end date (duration unknown)
#          Features are limited to phase and sponsor type, baseline
#          characteristics nominally fixed at study start (observed in
#          records retrieved in 2026). Registration-to-start lag is not
#          used: most trials were registered after they started, so their
#          lag was not known at study start.
#          1. Kaplan-Meier median duration by start era vs completed-only median
#          2. Time-based validation (train <= 2015, test 2016-2018): Weibull AFT
#             on censored data vs a log-duration regression fitted on completed
#             trials only, same features; concordance on the censored test set
#          3. Sensitivity to how terminated trials are treated
# Input:   app/data-raw/cache/oncology_trials_all_status.rds (script 10)
# Output:  app/models/survival_km_summary.csv, survival_km_curves.csv,
#          survival_validation.csv, survival_termination_check.csv
# Author:  Steven Ponce
# Date:    October 2026
# ============================================================================

library(here)
library(dplyr)
library(stringr)
library(survival)

# Same date parser as 02_download_and_clean.R
for (e in parse(here("scripts", "02_download_and_clean.R"))) {
  if (is.call(e) && identical(e[[1]], as.name("<-")) &&
      identical(as.character(e[[2]]), "parse_registry_date")) eval(e)
}

raw <- readRDS(here("app", "data-raw", "cache", "oncology_trials_all_status.rds"))
pull_date <- attr(raw, "pull_date")
months <- function(days) as.numeric(days) / 30.4375

running <- c("ACTIVE_NOT_RECRUITING", "RECRUITING", "ENROLLING_BY_INVITATION", "SUSPENDED")

base <- raw |>
  filter(study_type == "INTERVENTIONAL", primary_purpose == "TREATMENT") |>
  mutate(start = parse_registry_date(start_date),
         completion = parse_registry_date(completion_date),
         start_year = as.numeric(format(start, "%Y"))) |>
  filter(!is.na(start), start_year >= 1990, start_year <= 2018)

counts <- c(
  base = nrow(base),
  withdrawn_or_not_started = sum(base$overall_status %in% c("WITHDRAWN", "NOT_YET_RECRUITING")),
  unknown_status = sum(base$overall_status == "UNKNOWN")
)

# Finished trials with no end date have an unknown duration: excluded
counts["finished_without_end_date"] <- sum(base$overall_status %in% c("COMPLETED", "TERMINATED") &
                                             is.na(base$completion))

surv_data <- base |>
  filter(overall_status %in% c("COMPLETED", "TERMINATED", running)) |>
  filter(!(overall_status %in% c("COMPLETED", "TERMINATED") & is.na(completion))) |>
  mutate(
    # Older records often lack a completion-date type; a COMPLETED trial with a
    # completion date counts as finished (only an ESTIMATED date is not)
    event = as.integer(overall_status == "COMPLETED" & !is.na(completion) &
                         !(completion_date_type %in% "ESTIMATED")),
    end = case_when(
      event == 1 ~ completion,
      overall_status == "TERMINATED" & !is.na(completion) &
        !(completion_date_type %in% "ESTIMATED") ~ completion,
      TRUE ~ pull_date
    ),
    time_months = months(end - start),
    phase_simple = case_when(
      phase == "PHASE2" ~ "Phase 2",
      phase == "PHASE3" ~ "Phase 3",
      str_detect(phase, "PHASE1") ~ "Phase 1/2",
      str_detect(phase, "PHASE2, PHASE3") ~ "Phase 2/3",
      TRUE ~ "Other"
    ) |> factor(levels = c("Phase 2", "Phase 3", "Phase 1/2", "Phase 2/3")),
    sponsor_simple = case_when(
      sponsor_class == "INDUSTRY" ~ "Industry",
      sponsor_class == "NIH" ~ "NIH",
      sponsor_class %in% c("NETWORK", "OTHER_GOV", "FED") ~ "Government/Network",
      TRUE ~ "Academic/Other"
    ) |> factor(levels = c("Industry", "Academic/Other", "NIH", "Government/Network")),
    registration_to_start = as.numeric(start - as.Date(first_submit_date)),
    era = if_else(start_year <= 2015, "Started 1990-2015", "Started 2016-2018")
  ) |>
  filter(!is.na(phase_simple), !is.na(time_months), time_months > 0)


cat("Cohort:", nrow(surv_data), "trials;", sum(surv_data$event), "completed,",
    sum(1 - surv_data$event), "censored\n")
print(table(surv_data$overall_status, surv_data$era))

# ---------------------------------------------------------------------------
# 1. Kaplan-Meier by start era vs completed-only medians
# ---------------------------------------------------------------------------
km <- survfit(Surv(time_months, event) ~ era, data = surv_data)
km_tab <- summary(km)$table
km_summary <- surv_data |>
  group_by(era) |>
  summarise(n = n(), n_completed = sum(event), n_censored = sum(1 - event),
            n_still_running = sum(overall_status %in% running),
            completed_only_median = round(median(time_months[event == 1]), 1),
            .groups = "drop") |>
  mutate(km_median = round(unname(km_tab[paste0("era=", era), "median"]), 1),
         km_median_lcl = round(unname(km_tab[paste0("era=", era), "0.95LCL"]), 1),
         km_median_ucl = round(unname(km_tab[paste0("era=", era), "0.95UCL"]), 1),
         n_excluded_unknown_status = counts["unknown_status"],
         n_excluded_never_started = counts["withdrawn_or_not_started"],
         n_excluded_no_end_date = counts["finished_without_end_date"],
         pull_date = format(pull_date))
print(as.data.frame(km_summary))

# Monthly points of each KM curve for the app chart
grid <- 0:180
# Each curve stops at its longest follow-up: beyond that there is no data
km_curves <- bind_rows(lapply(names(km$strata), function(s) {
  era_s <- sub("era=", "", s)
  max_t <- max(surv_data$time_months[surv_data$era == era_s])
  sm <- summary(km[s], times = grid[grid <= max_t], extend = TRUE)
  data.frame(era = era_s, month = sm$time, still_running = round(sm$surv, 4))
}))

# ---------------------------------------------------------------------------
# 2. Time-based validation, phase and sponsor only
# ---------------------------------------------------------------------------
train <- surv_data |> filter(start_year <= 2015)
test  <- surv_data |> filter(start_year >= 2016)

aft <- survreg(Surv(time_months, event) ~ phase_simple + sponsor_simple,
               data = train, dist = "weibull")
naive <- lm(log(time_months) ~ phase_simple + sponsor_simple,
            data = train |> filter(event == 1))

test$pred_aft   <- predict(aft, newdata = test, type = "quantile", p = 0.5)  # median months
test$pred_naive <- exp(predict(naive, newdata = test))

cidx <- function(pred) {
  unname(concordance(Surv(time_months, event) ~ pred, data = test)$concordance)
}
km_test <- unname(summary(survfit(Surv(time_months, event) ~ 1, data = test))$table["median"])

validation <- data.frame(
  model = c("Weibull AFT, censored data", "Regression on completed trials only"),
  train_n = c(nrow(train), sum(train$event)),
  test_n = nrow(test),
  test_concordance = round(c(cidx(test$pred_aft), cidx(test$pred_naive)), 3),
  median_predicted_months = round(c(median(test$pred_aft), median(test$pred_naive)), 1),
  test_km_median_months = round(km_test, 1)
)
print(validation)

# ---------------------------------------------------------------------------
# 3. Sensitivity: how terminated trials are treated
# ---------------------------------------------------------------------------
# Sections 1-2 censor terminated trials at their stop date, which assumes
# termination says nothing about when the trial would have completed. Two
# alternative estimands, by start era:
#   (a) time to study end: completion OR termination counts as the event
#   (b) time to completion with termination as a competing event: the
#       cumulative incidence of completion (Aalen-Johansen); its median is the
#       month at which half of all trials have completed, if that happens
terminated_end <- surv_data$overall_status == "TERMINATED" & surv_data$end != pull_date
surv_data <- surv_data |>
  mutate(terminated_end = terminated_end,
         event_end = as.integer(event == 1 | terminated_end),
         status_cr = factor(case_when(event == 1 ~ "completed",
                                      terminated_end ~ "terminated",
                                      TRUE ~ "censored"),
                            levels = c("censored", "completed", "terminated")))

km_end <- summary(survfit(Surv(time_months, event_end) ~ era, data = surv_data))$table
cif <- survfit(Surv(time_months, status_cr) ~ era, data = surv_data)
cif_median <- sapply(seq_along(cif$strata), function(i) {
  idx <- (c(0, cumsum(cif$strata))[i] + 1):cumsum(cif$strata)[i]
  p_comp <- cif$pstate[idx, which(cif$states == "completed")]
  t <- cif$time[idx]
  if (any(p_comp >= 0.5)) t[which(p_comp >= 0.5)[1]] else NA_real_
})
names(cif_median) <- sub("era=", "", names(cif$strata))
cif_at <- function(m) sapply(seq_along(cif$strata), function(i) {
  idx <- (c(0, cumsum(cif$strata))[i] + 1):cumsum(cif$strata)[i]
  p_comp <- cif$pstate[idx, which(cif$states == "completed")]
  t <- cif$time[idx]
  if (min(t) > m) 0 else p_comp[max(which(t <= m))]
})

termination_check <- surv_data |>
  group_by(era) |>
  summarise(n = n(), n_terminated = sum(.data$terminated_end), .groups = "drop") |>
  mutate(
    km_median_terminated_censored = round(unname(km_tab[paste0("era=", era), "median"]), 1),
    km_median_study_end = round(unname(km_end[paste0("era=", era), "median"]), 1),
    cif_median_completion = round(unname(cif_median[era]), 1),
    completed_by_60mo = round(cif_at(60), 3),
    completed_by_84mo = round(cif_at(84), 3)
  )
print(as.data.frame(termination_check))

write.csv(termination_check, here("app", "models", "survival_termination_check.csv"), row.names = FALSE)
write.csv(km_summary, here("app", "models", "survival_km_summary.csv"), row.names = FALSE)
write.csv(km_curves,  here("app", "models", "survival_km_curves.csv"),  row.names = FALSE)
write.csv(validation, here("app", "models", "survival_validation.csv"), row.names = FALSE)
cat("\nSaved: app/models/survival_km_summary.csv, survival_km_curves.csv, survival_validation.csv\n")
