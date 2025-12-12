#INCIDENCE
print("Preparing incidence rates for Joinpoint modelling software")

joinpoint <- inc_all_plot |>
  as_tibble() |>
  filter(estimate_name %in% c("incidence_100000_pys", "incidence_100000_pys_95CI_lower", "incidence_100000_pys_95CI_upper")) |>
  dplyr::select(estimate_value, estimate_name, denominator_age_group, denominator_sex, additional_level) |>
  separate(additional_level, into = c("start_date", "end_date", "interval"), sep = " &&& ") %>%
  mutate(
    year = year(ymd(start_date)),
    rate = as.numeric(estimate_value)
  ) |>
  dplyr::select(year, denominator_age_group, denominator_sex, estimate_name, rate) |>
  pivot_wider(
    names_from = estimate_name,
    values_from = rate
  ) |>
  mutate(
    CI_lower = as.numeric(incidence_100000_pys_95CI_lower),
    CI_upper = as.numeric(incidence_100000_pys_95CI_upper),
    SE = (CI_upper - CI_lower) / (2 * 1.96)
  ) |>
  rename(rate=incidence_100000_pys) |>
  dplyr::select(-CI_lower, -CI_upper, -incidence_100000_pys_95CI_lower, -incidence_100000_pys_95CI_upper)

inc_sdi_clean <- inc_sdi |>
  filter(!strata_level %in% c("overall", "NA"))

joinpoint_sdi <- inc_sdi_clean |>
  as_tibble() |>
  filter(estimate_name %in% c("incidence_100000_pys", "incidence_100000_pys_95CI_lower", "incidence_100000_pys_95CI_upper")) |>
  dplyr::select(estimate_value, estimate_name, strata_level, additional_level) |>
  separate(additional_level, into = c("start_date", "end_date", "interval"), sep = " &&& ") %>%
  mutate(
    year = year(ymd(start_date)),
    rate = as.numeric(estimate_value)
  ) |>
  dplyr::select(year, strata_level, estimate_name, rate) |>
  pivot_wider(
    names_from = estimate_name,
    values_from = rate
  ) |>
  mutate(
    CI_lower = as.numeric(incidence_100000_pys_95CI_lower),
    CI_upper = as.numeric(incidence_100000_pys_95CI_upper),
    SE = (CI_upper - CI_lower) / (2 * 1.96)
  ) |>
  rename(rate=incidence_100000_pys) |>
  dplyr::select(-CI_lower, -CI_upper, -incidence_100000_pys_95CI_lower, -incidence_100000_pys_95CI_upper) |>
  mutate(denominator_age_group = "0 to 17") |>
  mutate(denominator_sex = "Both") |>
  rename(sdi = strata_level) |>
  arrange(
    denominator_age_group,
    denominator_sex,
    sdi,
    year
  )


# This we need it to export to the Joinpoint program
joinpoint <- joinpoint |>
  mutate(rate = str_replace(as.character(rate), "\\.", ",")) |>
  mutate(SE = str_replace(as.character(SE), "\\.", ","))

joinpoint_sdi <- joinpoint_sdi |>
  mutate(rate = str_replace(as.character(rate), "\\.", ",")) |>
  mutate(SE = str_replace(as.character(SE), "\\.", ","))

write_xlsx(joinpoint, path = here::here("Results/Incidence/APC_AAPC/aapc_crude_rates_age_sex.xlsx"))
write_xlsx(joinpoint_sdi, path = here::here("Results/Incidence/APC_AAPC/aapc_crude_rates_sdi.xlsx"))

#PREVALENCE
print("Preparing prevalence % for Joinpoint modelling software")
prev_all_plot <- prev_all |>
  left_join(settings(prev_all), by="result_id")

joinpoint_prev <- prev_all_plot |>
  as_tibble() |>
  filter(estimate_name %in% c("prevalence", "prevalence_95CI_lower", "prevalence_95CI_upper")) |>
  dplyr::select(estimate_value, estimate_name, denominator_age_group, denominator_sex, additional_level) |>
  separate(additional_level, into = c("start_date", "end_date", "interval"), sep = " &&& ") %>%
  mutate(
    year = year(ymd(start_date)),
    rate = as.numeric(estimate_value)*100
  ) |>
  dplyr::select(year, denominator_age_group, denominator_sex, estimate_name, rate) |>
  pivot_wider(
    names_from = estimate_name,
    values_from = rate
  ) |>
  mutate(
    CI_lower = as.numeric(prevalence_95CI_lower),
    CI_upper = as.numeric(prevalence_95CI_upper),
    SE = (CI_upper - CI_lower) / (2 * 1.96)
  ) |>
  rename(rate=prevalence) |>
  dplyr::select(-CI_lower, -CI_upper, -prevalence_95CI_lower, -prevalence_95CI_upper)

prev_sdi_clean <- prev_sdi |>
  filter(!strata_level %in% c("overall", "NA"))

joinpoint_sdi_prev <- prev_sdi_clean |>
  as_tibble() |>
  filter(estimate_name %in% c("prevalence", "prevalence_95CI_lower", "prevalence_95CI_upper")) |>
  dplyr::select(estimate_value, estimate_name, strata_level, additional_level) |>
  separate(additional_level, into = c("start_date", "end_date", "interval"), sep = " &&& ") %>%
  mutate(
    year = year(ymd(start_date)),
    rate = as.numeric(estimate_value)*100
  ) |>
  dplyr::select(year, strata_level, estimate_name, rate) |>
  pivot_wider(
    names_from = estimate_name,
    values_from = rate
  ) |>
  mutate(
    CI_lower = as.numeric(prevalence_95CI_lower),
    CI_upper = as.numeric(prevalence_95CI_upper),
    SE = (CI_upper - CI_lower) / (2 * 1.96)
  ) |>
  rename(rate=prevalence) |>
  dplyr::select(-CI_lower, -CI_upper, -prevalence_95CI_lower, -prevalence_95CI_upper) |>
  mutate(denominator_age_group = "0 to 17") |>
  mutate(denominator_sex = "Both") |>
  rename(sdi = strata_level) |>
  arrange(
    denominator_age_group,
    denominator_sex,
    sdi,
    year
  )


# This we need it to export to the Joinpoint program
joinpoint_prev <- joinpoint_prev |>
  mutate(rate = str_replace(as.character(rate), "\\.", ",")) |>
  mutate(SE = str_replace(as.character(SE), "\\.", ","))

joinpoint_sdi_prev <- joinpoint_sdi_prev |>
  mutate(rate = str_replace(as.character(rate), "\\.", ",")) |>
  mutate(SE = str_replace(as.character(SE), "\\.", ","))

write_xlsx(joinpoint_prev, path = here::here("Results/Prevalence/APC_AAPC/aapc_crude_rates_age_sex.xlsx"))
write_xlsx(joinpoint_sdi_prev, path = here::here("Results/Prevalence/APC_AAPC/aapc_crude_rates_sdi.xlsx"))
