# Attirtion -------
print("Plotting attrition")

summary_flow <- cdm$denominator_all |>
  summariseCohortAttrition(cohortId = 15) |> #all and both sexes
  plotCohortAttrition(show = "subjects")
summary_flow
# Save grViz object as PNG
rsvg_png(
  charToRaw(export_svg(summary_flow)),
  file = here::here("Results/Descriptive/Flow_chart.png"),
  width = 4000,  # 12 inches × 300 dpi
  height = 4000
)

# DESCRIPTIVE OVERALL --------
#table summarise characteristics for cohort 15 (all age groups and both sexes)
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

      TRUE ~ "Other/Unknown"
    )
  ) |>
  compute()  # Perform the final compute after all operations


results_summarise <- cdm$denominator_all_table1 |>
  summariseCharacteristics(
    cohortId = 1,
    otherVariables = c("sdi", "region") #play with estimates
  )

table1 <- results_summarise |>
tableCharacteristics(type = "flextable")

# Create a Word document
doc <- read_docx()

# Add the flextable to the document
doc <- doc %>%
  body_add_flextable(table1)

# Save the Word document
print(doc, target = "Results/Descriptive/table1_overall.docx")


# DESCRIPTIVE INC ASTHMA --------
cdm$table1_asthma_inc <- cdm$outcome_table1  |>
 left_join( # ADDING sdi
   cdm$observation |>
     filter(observation_source_value == "medea") |>
     filter(value_as_string %in% c("U1", "U2", "U3", "U4", "U5", "R", "0N")) |>
     select(person_id, value_as_string) |>
     rename(subject_id = person_id, medea = value_as_string),
   by = "subject_id"
 ) |>
  mutate(medea = ifelse(medea == "0N", NA, medea)) |>
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

      TRUE ~ "Other/Unknown"
    )
  ) |>
  compute()  # Perform the final compute after all operations


results_summarise_inc <- cdm$table1_asthma_inc |>
  summariseCharacteristics(
  otherVariables = c("medea", "region") #play with estimates
  )

table1_inc_asthma <- results_summarise_inc |>
  tableCharacteristics(type = "flextable")

# Create a Word document
doc <- read_docx()

# Add the flextable to the document
doc <- doc %>%
  body_add_flextable(table1_inc_asthma)

# Save the Word document
print(doc, target = "Results/Descriptive/table1_inc_asthma.docx")

