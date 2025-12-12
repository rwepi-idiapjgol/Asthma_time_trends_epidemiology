# Outcome imported from ATLAS------
print("Instanciating outcome cohort")

outcome_cohorts <- CDMConnector::readCohortSet(here::here("outcome_cohorts"))
cdm <- CDMConnector::generateCohortSet(cdm,
                                       outcome_cohorts,
                                       name = "outcome_table",
                                       overwrite=T)

#For table 1 I only want incident cases of asthma
cdm <- CDMConnector::generateCohortSet(cdm,
                                       outcome_cohorts,
                                       name = "outcome_table1",
                                       overwrite=T)

cdm$outcome_table1 <- cdm$outcome_table1 |>
  requirePriorObservation(365) |>
  requireDemographics(ageRange = c(0,17)) |>
  requireInDateRange(dateRange = c(as.Date("2010-01-01"), as.Date("2024-12-31"))) |>
  requireIsFirstEntry() |>
  compute()
