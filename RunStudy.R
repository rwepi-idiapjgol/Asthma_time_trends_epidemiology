# Create cdm reference ----
print("Creating CDM")
cdm <- cdmFromCon(
  con = db,
  cdmSchema = cdm_database_schema,
  writeSchema = results_database_schema,
  writePrefix = tolower(table_stem),
  cdmName = database_name
)

# min cell count
min_cell_count <- 5
source(here::here("Scripts/1_OutcomeCohorts.R"))
source(here::here("Scripts/2_Denominator.R"))
source(here::here("Scripts/3_Descriptives.R"))
source(here::here("Scripts/4_Incidence.R"))
source(here::here("Scripts/5_Prevalence.R"))
source(here::here("Scripts/6_AAPC.R"))
source(here::here("Scripts/7_IncidenceRateRatios.R"))
