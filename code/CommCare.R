# CommCare

# Inputs -----------------------------------------------------------------------

## Years of interest -----------------------------------------------------------

years <-
  data.frame("year" = 2010:2035)

## Parameters ------------------------------------------------------------------

parameters <-
  read_csv(
    here(
      "data",
      "CommCare",
      "raw",
      "parameters.csv"
    )
  )

## Reach -----------------------------------------------------------------------

reach <-
  read_csv(
    here(
      "data",
      "CommCare",
      "raw",
      "reach.csv"
    )
  ) %>%
  remove_empty

## Cost ------------------------------------------------------------------

in_cost <-
  read_csv(
    here(
      "data",
      "CommCare",
      "raw",
      "innovation_cost.csv"
    )
  ) %>%
  remove_empty

# Calculations -----------------------------------------------------------------

## Calculated parameters -------------------------------------------------------

# Why divide by 2?
parameters_c <-
  parameters %>%
  group_by(name) %>%
  summarise(
    value = mean(value)
  ) %>%
  pivot_wider %>%
  mutate(
    across(
      ends_with("effect_on_mortality"),
      ~ . / 2
    ),
    tetanus_effect = effect_on_tetanus_takeup * tetanus_effect_on_mortality,
    anc_effect = effect_on_anc * anc_effect_on_mortality,
    ifa_effect = effect_on_ifa * ifa_effect_on_mortality
  ) %>%
  rowwise %>%
  mutate(
    effect_on_mr = sum(c_across(ends_with("_effect")))
  ) %>%
  ungroup %>%
  mutate(
    deaths_averted_per_flw = effect_on_mr * preg_women_served_per_flw_year * neonatal_mr,
    life_years_saved_per_flw = deaths_averted_per_flw * life_years_per_child
  )

## Benefits --------------------------------------------------------------------

benefits <-
  reach %>%
  left_join(
    gdp_pc %>%
      select(year, gdp_pc = india)
  ) %>%
  mutate(
    life_years_saved = av_flws_per_calendar_year * parameters_c$life_years_saved_per_flw,
    benefits_usd = life_years_saved * gdp_pc,
    benefits_op = life_years_saved * global_parameters$daly_op / global_parameters$op_unit
  )
  
  
## Operating costs -------------------------------------------------------------

op_cost <-
  reach %>%
  mutate(
    unit_cost = case_when(
      year < 2021 ~ parameters_c$unit_cost_pre2021,
      year >= 2021 ~ parameters_c$unit_cost_2021onward
    ),
    op_cost_usd = av_flws_per_calendar_year * unit_cost
  )

## Innovation costs ------------------------------------------------------------

in_cost_c <-
  in_cost %>%
  group_by(year, investment) %>%
  summarise(value = sum(value * share, na.rm = TRUE)) %>%
  pivot_wider(names_from = investment) %>%
  rename_with(
    ~ paste0(.x, "_investment"),
    ends_with("div")
  )

# Save data --------------------------------------------------------------------

CommCare <-
  list(
    parameters = parameters_c,
    reach = reach,
    benefits = benefits,
    operating_costs = op_cost,
    innovation_costs = in_cost_c
  )

CommCare %>%
  iwalk(
    ~ write_csv(
      .x, 
      here(
        "data",
        "CommCare",
        "final",
        paste0(.y, ".csv")
      )
    )
  )

CommCare %>%
  write_rds(
    here(
      "data",
      "CommCare",
      "final",
      "all_data.rds"
    )
  )
