#renv::activate()
#renv::restore()

# Load packages ------
library(SqlRender)
library(CirceR)
library(CDMConnector)
library(IncidencePrevalence)
library(here)
library(DBI)
library(dbplyr)
library(tidyr)
library(ggplot2)
library(gt)
library(dplyr)
library(readr)
library(log4r)
library(stringr)
library(RPostgres)
library(PatientProfiles)
library(CohortCharacteristics)
library(DiagrammeRsvg)
library(rsvg)
library(flextable)
library(gt)
library(officer)
library(writexl)
library(lubridate)
library(purrr)
library(broom)
library(tibble)
library(CohortConstructor)

# Connection details ------

# Database name
database_name <- "SIDIAP"

# Connection details
server_dbi <- Sys.getenv("DB_SERVER_DBI_24")
user <- Sys.getenv("DB_USER_AG")
password <- Sys.getenv("DB_PASS_AG")
port <- Sys.getenv("DB_PORT")
host <- Sys.getenv("DB_HOST")

db <- dbConnect(RPostgres::Postgres(),
                dbname = server_dbi,
                port = port,
                host = host,
                user = user,
                password = password
)

cdm_database_schema <- "omop_cmbd"
results_database_schema <- "results_cmbd"

# cohort stem where cohorts will be instantiated
table_stem <- "apc_asma"

# Run study ------

source(here::here("RunStudy.R"))
#takes about 30 minutes to run in CMBD
print("Thanks for running this study!")

