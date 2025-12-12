# IR------
print("Working on yearly Incidence all and by sex and age")
inc_all <- estimateIncidence(
  cdm = cdm,
  denominatorTable = "denominator_all",
  outcomeTable = "outcome_table",
  outcomeCohortId = 1,
  interval = "years",
  repeatedEvents = FALSE,
  outcomeWashout = Inf,
  completeDatabaseIntervals = TRUE
)

table_all <- tableIncidence(inc_all, type="tibble")
table_all <- table_all %>%
  mutate(`[header_name]Estimate name\n[header_level]Incidence 100,000 person-years [95% CI]` = str_replace_all(
    `[header_name]Estimate name\n[header_level]Incidence 100,000 person-years [95% CI]`,
    "\\s*-\n\\s*",  # matches newline and spaces around the dash
    "-"
  ))

table_all <- table_all |>
  mutate(
    Year = year(as.Date(`Incidence start date`))
  ) |>
  mutate(`Denominator age group` = factor(
    `Denominator age group`,
    levels = c("0 to 4", "5 to 9", "10 to 14", "15 to 17", "0 to 17")
  )) |>
  arrange(`Denominator age group`) |>
  group_by(`Denominator age group`) |>
  mutate(
    age_group_display = ifelse(row_number() == 1,
                               as.character(`Denominator age group`),
                               "")
  ) |>
  ungroup() |>
    group_by(`Denominator sex`) |>
  mutate(
    sex_display = ifelse(row_number() == 1,
                               as.character(`Denominator sex`),
                               "")
  ) |>
  ungroup() |>
  select(
    -`Database name`,
    -`Outcome cohort name`,
    -`Denominator age group`,
    -`Incidence start date`,
    -`Incidence end date`,
    -`Denominator sex`
  ) |>
  relocate(Year, age_group_display, sex_display)


tableIncidenceAttrition(inc_all) #36,680 excluded because were prevalent cases

print("Working on overall Incidence all and by sex and age")
inc_overall <- estimateIncidence(
  cdm = cdm,
  denominatorTable = "denominator_all",
  outcomeTable = "outcome_table",
  outcomeCohortId = 1,
  interval = "overall",
  repeatedEvents = FALSE,
  outcomeWashout = Inf,
  completeDatabaseIntervals = TRUE
)

tableIncidenceAttrition(inc_overall)

table_overall <- tableIncidence(inc_overall, type="tibble")
table_overall <- table_overall %>%
  mutate(`[header_name]Estimate name\n[header_level]Incidence 100,000 person-years [95% CI]` = str_replace_all(
    `[header_name]Estimate name\n[header_level]Incidence 100,000 person-years [95% CI]`,
    "\\s*-\n\\s*",  # matches newline and spaces around the dash
    "-"
  ))

print("Working on annual Incidence by sdi")

inc_sdi <- estimateIncidence(
  cdm = cdm,
  denominatorTable = "denominator_sdi",
  outcomeTable = "outcome_table",
  interval = "years",
  repeatedEvents = FALSE,
  outcomeWashout = Inf,
  completeDatabaseIntervals = TRUE,
  strata=list("sdi")
)

table_sdi <- tableIncidence(inc_sdi, type = "tibble")
table_sdi <- table_sdi %>%
  mutate(`[header_name]Estimate name\n[header_level]Incidence 100,000 person-years [95% CI]` = str_replace_all(
    `[header_name]Estimate name\n[header_level]Incidence 100,000 person-years [95% CI]`,
    "\\s*-\n\\s*",  # matches newline and spaces around the dash
    "-"
  ))

table_sdi <- table_sdi |>
  mutate(
    Year = year(as.Date(`Incidence start date`))
  ) |>
  mutate(Sdi = factor(
    Sdi,
    levels = c("overall", "NA", "R", "Q1", "Q2", "Q3", "Q4", "Q5")
  )) |>
  arrange(Sdi) |>
  group_by(Sdi) |>
  mutate(
    sdi_display = ifelse(row_number() == 1,
                               as.character(Sdi),
                               "")
  ) |>
  ungroup() |>
  select(
    -`Database name`,
    -`Outcome cohort name`,
    -`Incidence start date`,
    -`Incidence end date`,
    -`Denominator age group`,
    -`Denominator sex`,
    - Sdi
  ) |>
  relocate(Year, sdi_display)

print("Working on overall Incidence by sdi")

inc_sdi_overall <- estimateIncidence(
  cdm = cdm,
  denominatorTable = "denominator_sdi",
  outcomeTable = "outcome_table",
  interval = "overall",
  repeatedEvents = FALSE,
  outcomeWashout = Inf,
  completeDatabaseIntervals = TRUE,
  strata=list("sdi")
)

table_sdi_overall <- tableIncidence(inc_sdi_overall, type = "tibble")
table_sdi_overall <- table_sdi_overall %>%
  mutate(`[header_name]Estimate name\n[header_level]Incidence 100,000 person-years [95% CI]` = str_replace_all(
    `[header_name]Estimate name\n[header_level]Incidence 100,000 person-years [95% CI]`,
    "\\s*-\n\\s*",  # matches newline and spaces around the dash
    "-"
  ))

print("Saving results")

 # Create a list of dataframes with corresponding sheet names
 datasets <- list(
   "Annual" = table_all,
   "Overall" = table_overall,
   "sdi annual" = table_sdi,
   "sdi overall"=table_sdi_overall
 )

 print("Export incidence")

 # Write to an Excel file with multiple sheets
 write_xlsx(datasets, path = here::here("Results/Incidence/IR_numbers.xlsx"))

# Plots ------
#overall

print("Plotting overall, age, sex")
settings(inc_all) |> glimpse()

inc_all_plot <- inc_all |>
  left_join(settings(inc_all), by="result_id")

age_order <- c("0 to 17", "0 to 4", "5 to 9", "10 to 14", "15 to 17")

inc_all_plot <- inc_all_plot |>
  mutate(denominator_age_group_ordered = factor(denominator_age_group, levels = age_order, ordered = TRUE),
         Sex = denominator_sex)

plot_all <- plotIncidence(inc_all_plot, colour = "denominator_sex", facet="denominator_age_group_ordered", ribbon = TRUE) +
  scale_fill_brewer(palette="Dark2") + scale_color_brewer(palette="Dark2") +
  theme(
    panel.grid.major.x = element_blank(),   # Remove vertical grid lines
    panel.grid.minor.x = element_blank(),   # Remove minor vertical grid lines
    panel.grid.major.y = element_line(linewidth = 0.5, linetype = 'dashed', colour = "grey80"), # Major horizontal grid lines
    panel.grid.minor.y = element_line(linewidth = 0.5, linetype = 'dashed', colour = "grey80"),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    panel.border = element_blank(),
    strip.background = element_blank(),
    legend.position = "none"
  )

ggsave(here::here("Results/Incidence/IR_all.png"), plot = plot_all, width = 10, height = 6, bg = "white")

#sdi

print("Plotting sdi")

inc_sdi |> glimpse()

inc_sdi_clean <- inc_sdi |>
  filter(!strata_level %in% c("overall", "NA", "R"))

plot_sdi <- plotIncidence(inc_sdi_clean, colour = "sdi", ribbon = TRUE) +
  scale_fill_brewer(palette="Dark2") + scale_color_brewer(palette="Dark2") +
  theme(
    panel.grid.major.x = element_blank(),   # Remove vertical grid lines
    panel.grid.minor.x = element_blank(),   # Remove minor vertical grid lines
    panel.grid.major.y = element_line(linewidth = 0.5, linetype = 'dashed', colour = "grey80"), # Major horizontal grid lines
    panel.grid.minor.y = element_line(linewidth = 0.5, linetype = 'dashed', colour = "grey80"),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    panel.border = element_blank()          # <- Removes the outer border
    )

ggsave(here::here("Results/Incidence/IR_sdi.png"), plot = plot_sdi, width = 10, height = 6, bg = "white")


