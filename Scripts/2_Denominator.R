# Generate a denominator population
# All ---------------------------------------------------------------
print("Generating denominator for overall population")

cdm <- generateDenominatorCohortSet(
  cdm = cdm,
  name = "denominator_all",
  cohortDateRange = as.Date(c("2010-01-01", "2024-12-31")),
  ageGroup = list(c(0,4), c(5,9), c(10,14), c(15,17), c(0,17)),
  sex = c("Female", "Male", "Both"),
  daysPriorObservation = 365
)
# Medea ------------------------------------------------------------------------
print("Generating denominator for SES stratification")

cdm <- generateDenominatorCohortSet(
  cdm = cdm,
  name = "denominator_sdi",
  cohortDateRange = as.Date(c("2010-01-01", "2024-12-31")),
  ageGroup = list(c(0,17)),
  daysPriorObservation = 365
)

cdm$denominator_sdi <- cdm$denominator_sdi |>
  left_join( # ADDING MEDEA
    cdm$observation |>
      filter(observation_source_value == "medea") |>
      filter(value_as_string %in% c("U1", "U2", "U3", "U4", "U5", "R", "0N")) |>
      select(person_id, value_as_string) |>
      rename(subject_id = person_id, sdi = value_as_string),
    by = "subject_id"
  ) |>
  mutate(sdi = ifelse(sdi == "0N" | is.na(sdi), "NA", sdi)) |>
  mutate(
    sdi = case_when(
      sdi %in% c("U1", "U2", "U3", "U4", "U5") ~ replace(sdi, 'U', 'Q'),
      TRUE ~ sdi
    )
  ) |>
  compute()





