# Attirtion -------
print("Plotting attrition")

summary_flow <- cdm$denominator_all |>
  summariseCohortAttrition(cohortId = 15)

# custom labels
reason_labels <- c(
  "1" = "Individuals in database",
  "2" = "Missing year of birth",
  "3" = "Missing sex",
  "4" = "Never aged <18 during 2010–2024",
  "5" = "No observation during 2010–2024",
  "6" = "Observed but never aged <18 while observed",
  "7" = "Less than 12 months prior observation",
  "10" = "No eligible time after applying age and prior observation criteria"
)

# Replace the strata_level column
summary_flow <- summary_flow %>%
  mutate(strata_level = reason_labels[additional_level])


summary_flow <- plotCohortAttrition(summary_flow, show = "subjects")

# Save grViz object as PNG
rsvg_png(
  charToRaw(export_svg(summary_flow)),
  file = here::here("Results/Descriptive/Flow_chart.png"),
  width = 4000,  # 12 inches × 300 dpi
  height = 4000
)

# DESCRIPTIVE OVERALL --------
#table summarise characteristics
cdm$denominator_all_table1 <- cdm$denominator_sdi |>
  left_join( # ADDING NATIONALITY (both sdi and nationality in one join)
    cdm$observation |>
      filter(observation_source_value == "agr_nationality") |>
      select(person_id, value_as_string) |>
      rename(subject_id = person_id, nationality = value_as_string),
    by = "subject_id"
  ) |>
  mutate(
    region = case_when(
      nationality == "Espanya" ~ "Spain",

      nationality %in% c(
        "Europa occidental", "Europa oriental",
        "Europa septentrional", "Europa meridional"
      ) ~ "Europe (other than Spain)",

      nationality %in% c(
        "Amèrica del Nord", "Amèrica del Sud",
        "Amèrica central", "Carib"
      ) ~ "America",

      nationality %in% c(
        "Àsia occidental", "Àsia central", "Àsia oriental",
        "Àsia sud-oriental", "Àsia meridional"
      ) ~ "Asia",

      nationality %in% c(
        "Àfrica septentrional", "Àfrica occidental",
        "Àfrica central", "Àfrica oriental", "Àfrica meridional"
      ) ~ "Africa",

      nationality %in% c(
        "Austràlia i Nova Zelanda", "Polinèsia",
        "Melanèsia", "Micronèsia"
      ) ~ "Oceania",

      TRUE ~ "Spain"
    )
  ) |>
  compute()


results_summarise <- cdm$denominator_all_table1 |>
  summariseCharacteristics(
    cohortId = 1,
    ageGroup = list(c(0,4), c(5,9), c(10,14), c(15,17)),
    otherVariables = c("sdi", "region")
  )

table1 <- results_summarise |>
tableCharacteristics(type = "flextable")

# Create a Word document
doc <- read_docx()

# Add the flextable to the document
doc <- doc %>%
  body_add_flextable(table1)

# Save the Word document
print(doc, target = "Results/Descriptive/table1_overall_qmedea.docx")


# DESCRIPTIVE INC ASTHMA --------
cdm$table1_asthma_inc <- cdm$outcome_table1  |>
 left_join( # ADDING sdi
   cdm$observation |>
     filter(observation_source_value == "qmedea11") |>
     filter(value_as_string %in% c("U1", "U2", "U3", "U4", "U5", "R", "0N")) |>
     select(person_id, value_as_string) |>
     rename(subject_id = person_id, qmedea11 = value_as_string),
   by = "subject_id"
 ) |>
  mutate(qmedea11 = ifelse(qmedea11 == "0N", NA, qmedea11)) |>
  left_join( # ADDING NATIONALITY (both sdi and nationality in one join)
    cdm$observation |>
      filter(observation_source_value == "agr_nationality") |>
      select(person_id, value_as_string) |>
      rename(subject_id = person_id, nationality = value_as_string),
    by = "subject_id"
  ) |>
  mutate(
    region = case_when(
      nationality == "Espanya" ~ "Spain",

      nationality %in% c(
        "Europa occidental", "Europa oriental",
        "Europa septentrional", "Europa meridional"
      ) ~ "Europe (other than Spain)",

      nationality %in% c(
        "Amèrica del Nord", "Amèrica del Sud",
        "Amèrica central", "Carib"
      ) ~ "America",

      nationality %in% c(
        "Àsia occidental", "Àsia central", "Àsia oriental",
        "Àsia sud-oriental", "Àsia meridional"
      ) ~ "Asia",

      nationality %in% c(
        "Àfrica septentrional", "Àfrica occidental",
        "Àfrica central", "Àfrica oriental", "Àfrica meridional"
      ) ~ "Africa",

      nationality %in% c(
        "Austràlia i Nova Zelanda", "Polinèsia",
        "Melanèsia", "Micronèsia"
      ) ~ "Oceania",

      TRUE ~ "Spain"
    )
  ) |>
  compute()

results_summarise_inc <- cdm$table1_asthma_inc |>
  summariseCharacteristics(
  ageGroup = list(c(0,4), c(5,9), c(10,14), c(15,17)),
  otherVariables = c("qmedea11", "region")
  )

table1_inc_asthma <- results_summarise_inc |>
  tableCharacteristics(type = "flextable")

# Create a Word document
doc <- read_docx()

# Add the flextable to the document
doc <- doc %>%
  body_add_flextable(table1_inc_asthma)

# Save the Word document
print(doc, target = "Results/Descriptive/table1_inc_asthma_qmedea.docx")

