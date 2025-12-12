# SEX AND AGE ---------------

# Clean and prepare the data ---
print("Preparing dataset for IRR by sex and age")

df_clean <- table_overall %>%
  mutate(
    person_years = as.numeric(str_remove_all(`[header_name]Estimate name\n[header_level]Person-years`, ",")),
    outcomes = as.numeric(str_remove_all(`[header_name]Estimate name\n[header_level]Outcome (N)`, ",")),
    incidence_rate = (outcomes / person_years) * 100000
  )

print("Calculating IRR by sex")

# IRR Male vs Female (with CI) ---
irr_sex <- df_clean %>%
  filter(`Denominator sex` %in% c("Male", "Female"),
         `Denominator age group` %in% c("0 to 17")) %>%
  select(`Denominator age group`, `Denominator sex`, person_years, outcomes) %>%
  pivot_wider(names_from = `Denominator sex`, values_from = c(person_years, outcomes)) %>%
  mutate(
    IR_Male = outcomes_Male / person_years_Male,
    IR_Female = outcomes_Female / person_years_Female,
    IRR_Male_vs_Female = IR_Male / IR_Female,
    SE_log_IRR = sqrt(1 / outcomes_Male + 1 / outcomes_Female),
    CI_lower = exp(log(IRR_Male_vs_Female) - 1.96 * SE_log_IRR),
    CI_upper = exp(log(IRR_Male_vs_Female) + 1.96 * SE_log_IRR)
  )


# IRR for Age 15–17 vs (0–4, 5–9, 10–14), sex = Both ---
print("Calculating IRR by age")

# Define relevant age groups
age_groups_to_compare <- c("0 to 4", "5 to 9", "10 to 14", "15 to 17")

irr_age_data <- df_clean %>%
  filter(`Denominator age group` %in% age_groups_to_compare,
         `Denominator sex` == "Both") %>%
  select(`Denominator age group`, person_years, outcomes)

# Extract reference group: 15–17
ref <- irr_age_data %>%
  filter(`Denominator age group` == "15 to 17")

# Compare others to 15–17
irr_age_comparison <- irr_age_data %>%
  filter(`Denominator age group` != "15 to 17") %>%
  mutate(
    IR_15_17 = ref$outcomes / ref$person_years,
    IR_group = outcomes / person_years,
    IRR_vs_15_17 = IR_group / IR_15_17,
    SE_log_IRR = sqrt(1 / outcomes + 1 / ref$outcomes),
    CI_lower = exp(log(IRR_vs_15_17) - 1.96 * SE_log_IRR),
    CI_upper = exp(log(IRR_vs_15_17) + 1.96 * SE_log_IRR)
  )

irr_age_comparison

# sdi ---------------
print("Preparing dataset for IRR by SES")

# Assign the dataset to a working variable
df_sdi_overall <- table_sdi_overall

# Create a function to extract person-years and outcomes for each stratum
get_values <- function(stratum_name) {

  row <- df_sdi_overall %>%
    filter(Sdi == stratum_name)

  person_years <- row %>%
    pull(`[header_name]Estimate name\n[header_level]Person-years`) %>%
    str_remove_all(",") %>%
    as.numeric()

  outcomes <- row %>%
    pull(`[header_name]Estimate name\n[header_level]Outcome (N)`) %>%
    str_remove_all(",") %>%
    as.numeric()

  tibble(
    stratum = stratum_name,
    person_years = person_years,
    outcomes = outcomes
  )
}

# Define strata to include
strata <- c("Q1", "Q2", "Q3", "Q4", "Q5", "R")

# Extract values for all strata
df_strata <- map_dfr(strata, get_values)

print("Calculatin IRR by sdi")

# Calculate incidence rate (IR)
df_strata <- df_strata %>%
  mutate(IR = outcomes / person_years)

# Calculate IRR vs Q1 with 95% confidence intervals
ref <- df_strata %>% filter(stratum == "Q1")

df_irr <- df_strata %>%
  filter(stratum != "Q1") %>%
  mutate(
    IRR_vs_Q1 = IR / ref$IR,
    SE_log_IRR = sqrt(1 / ref$outcomes + 1 / outcomes),
    CI_lower = exp(log(IRR_vs_Q1) - 1.96 * SE_log_IRR),
    CI_upper = exp(log(IRR_vs_Q1) + 1.96 * SE_log_IRR)
  )

# Optional: View final IRR table
df_irr

# COMBINE THE TWO DATASETS -------------------
# Combine irr_sex, irr_age_comparison, and df_irr into one data frame
# Ensure the columns are standardized for easy merging and plotting
print("Combine IRR and export")

# Standardize column names
irr_sex <- irr_sex %>%
  select(
    group = `Denominator age group`,
    IRR = IRR_Male_vs_Female,
    CI_lower = CI_lower,
    CI_upper = CI_upper
  ) %>%
  mutate(
    type = "Sex",
    group = if_else(group == "0 to 17", "Males", group)  # Rename "0 to 17" to "Males"
  )

irr_age_comparison <- irr_age_comparison %>%
  select(
    group = `Denominator age group`,
    IRR = IRR_vs_15_17,
    CI_lower = CI_lower,
    CI_upper = CI_upper
  ) %>%
  mutate(type = "Age Groups")

df_irr <- df_irr %>%
  select(
    group = stratum,
    IRR = IRR_vs_Q1,
    CI_lower = CI_lower,
    CI_upper = CI_upper
  ) %>%
  mutate(type = "sdi")

# Combine all three datasets into one
combined_df <- bind_rows(irr_sex, irr_age_comparison, df_irr)

# View the combined dataset
head(combined_df)

combined_df$group <- factor(
  combined_df$group,
  levels = c(
    "R", "Q5", "Q4", "Q3", "Q2",
    "10 to 14", "5 to 9", "0 to 4",
    "Males"
  )
)

# PLOT IN A GGPLOT
# Plot IRR with confidence intervals


IRR_plot <- ggplot(combined_df, aes(x = IRR, y = group, color = type, group = type)) +
  geom_point(size = 2) +
  geom_errorbarh(aes(xmin = CI_lower, xmax = CI_upper), height = 0.2) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "black", linewidth = 1) +
  geom_text(
    aes(
      label = sprintf("IRR = %.2f [%.2f–%.2f]", IRR, CI_lower, CI_upper),
      x = CI_upper + 0.02
    ),
    color = "black",
    size = 3.5,
    vjust = -0.5,
    hjust = 0,
    show.legend = FALSE
  ) +
  labs(
    title = "IRR with 95% Confidence Intervals",
    x = "IRR",
    y = "Group",
    color = "Comparison Type"
  ) +
  scale_fill_brewer(palette = "Dark2") +
  scale_color_brewer(palette = "Dark2") +
  coord_cartesian(xlim = c(0, max(combined_df$CI_upper) + 0.3), clip = "off") +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(linewidth = 0.5, linetype = 'dashed', colour = "grey80"),
    panel.grid.minor.y = element_blank(),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    plot.margin = margin(5.5, 50, 5.5, 5.5, "pt")  # extra right margin
  )


IRR_plot
ggsave(here::here("Results/Incidence/IRR.png"), plot = IRR_plot, width = 10, height = 6, bg = "white")


