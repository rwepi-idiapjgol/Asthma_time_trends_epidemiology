# Prevalence --------------------------------------------------------------
print("Working on annual Prevalence in all participants and by age and sex")
prev_all <- estimatePointPrevalence(
  cdm = cdm,
  denominatorTable = "denominator_all",
  outcomeTable="outcome_table",
  interval = "years",
  timePoint = "end"
  )

table_prev_all <- tablePrevalence(prev_all, type="tibble")
tablePrevalenceAttrition(prev_all)

# Function to convert to %, I don't think there is an easier way
add_prevalence_ci <- function(data,
                              outcome_col = "[header_name]Estimate name\n[header_level]Outcome (N)",
                              denom_col   = "[header_name]Estimate name\n[header_level]Denominator (N)") {
  data %>%
    mutate(
      outcome_n = as.numeric(gsub(",", "", .data[[outcome_col]])),
      denom_n   = as.numeric(gsub(",", "", .data[[denom_col]])),
      prev      = outcome_n / denom_n,
      se        = sqrt(prev * (1 - prev) / denom_n),
      z         = 1.96,
      lower     = prev - z * se,
      upper     = prev + z * se,
      `Prevalence [95% CI] (%)` = paste0(
        round(prev * 100, 2), "% (",
        round(lower * 100, 2), "–",
        round(upper * 100, 2), "%)"
      )
    ) %>%
    select(-outcome_n, -denom_n, -prev, -se, -z, -lower, -upper)
}
table_prev_all <- add_prevalence_ci(table_prev_all)

table_prev_all <- table_prev_all |>
  mutate(
    Year = year(as.Date(`Prevalence start date`))
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
    -`Prevalence start date`,
    -`Prevalence end date`,
    -`Denominator sex`
  ) |>
  relocate(Year, age_group_display, sex_display)


print("Working on overall Prevalence in all participants and by age and sex")
prev_overall <- estimatePeriodPrevalence(
  cdm = cdm,
  denominatorTable = "denominator_all",
  outcomeTable="outcome_table",
  interval = "overall"
)

table_prev_overall <- tablePrevalence(prev_overall, type="tibble")
table_prev_overall <- add_prevalence_ci(table_prev_overall)

print("Working on annual Prevalence by sdi")
prev_sdi <- estimatePointPrevalence(
  cdm = cdm,
  denominatorTable = "denominator_sdi",
  outcomeTable="outcome_table",
  interval = "years",
  timePoint = "end",
  strata=list("sdi")
)

table_prev_sdi <- tablePrevalence(prev_sdi, type="tibble")
table_prev_sdi <- add_prevalence_ci(table_prev_sdi)

table_prev_sdi <- table_prev_sdi |>
  mutate(
    Year = year(as.Date(`Prevalence start date`))
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
    -`Prevalence start date`,
    -`Prevalence end date`,
    -`Denominator age group`,
    -`Denominator sex`,
    - Sdi
  ) |>
  relocate(Year, sdi_display)


print("Working on overall Prevalence by sdi")
prev_sdi_overall <- estimatePeriodPrevalence(
  cdm = cdm,
  denominatorTable = "denominator_sdi",
  outcomeTable="outcome_table",
  interval = "overall",
  strata=list("sdi")
)

table_prev_sdi_overall <- tablePrevalence(prev_sdi_overall, type="tibble")
table_prev_sdi_overall <- add_prevalence_ci(table_prev_sdi_overall)

print("Saving results")
library(writexl)

# Create a list of dataframes with corresponding sheet names
datasets <- list(
  "Annual all" = table_prev_all,
  "Overall all" = table_prev_overall,
  "Annual sdi" = table_prev_sdi,
  "Overall sdi" = table_prev_sdi_overall
)

print("Export prevalence")

# Write to an Excel file with multiple sheets
write_xlsx(datasets, path = here::here("Results/Prevalence/PP_numbers.xlsx"))

# Plots ------
#overall
prev_all_plot <- prev_all |>
  left_join(settings(prev_all), by="result_id")

age_order <- c("0 to 17", "0 to 4", "5 to 9", "10 to 14", "15 to 17")

prev_all_plot <- prev_all_plot |>
  mutate(denominator_age_group_ordered = factor(denominator_age_group, levels = age_order, ordered = TRUE),
         Sex = denominator_sex)|>
  filter(!denominator_sex=="Both")

plot_prev_all <- plotPrevalence(prev_all_plot, colour = "denominator_sex", facet="denominator_age_group_ordered", ribbon = TRUE) +
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

ggsave(here::here("Results/Prevalence/Prev_all.png"), plot = plot_prev_all, width = 10, height = 6, bg = "white")

#sdi

print("Plotting sdi")

prev_sdi |> glimpse()

prev_sdi_clean <- prev_sdi |>
  filter(!strata_level %in% c("overall", "NA", "R"))

plot_sdi <- plotPrevalence(prev_sdi_clean, colour = "sdi", ribbon = TRUE) +
  scale_fill_brewer(palette="Dark2") + scale_color_brewer(palette="Dark2") +
  theme(
    panel.grid.major.x = element_blank(),   # Remove vertical grid lines
    panel.grid.minor.x = element_blank(),   # Remove minor vertical grid lines
    panel.grid.major.y = element_line(linewidth = 0.5, linetype = 'dashed', colour = "grey80"), # Major horizontal grid lines
    panel.grid.minor.y = element_line(linewidth = 0.5, linetype = 'dashed', colour = "grey80"),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    panel.border = element_blank()          # <- Removes the outer border
  )

ggsave(here::here("Results/Prevalence/Prev_sdi.png"), plot = plot_sdi, width = 10, height = 6, bg = "white")


