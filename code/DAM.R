# Process Digital Attendance Monitoring data

# Inputs -----------------------------------------------------------------------

## Years of interest -----------------------------------------------------------

years <-
  data.frame("year" = 2010:2013)

## Parameters ------------------------------------------------------------------

parameters <-
  read_csv(
    here(
      "data",
      "DAM",
      "raw",
      "parameters.csv"
    )
  ) %>% 
  select(value, name) %>%
  pivot_wider

## Reach -----------------------------------------------------------------------

reach <-
  read_csv(
    here(
      "data",
      "DAM",
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
      "DAM",
      "raw",
      "innovation_cost.csv"
    )
  )

# Calculations -----------------------------------------------------------------

## Calculated parameters -------------------------------------------------------

parameters_c <-
  parameters %>%
  mutate(
    # Why?
    across(
      c(effect_on_mortality, neonatal_mr_reduction),
      ~ . / 2
    ),
    preg_per_village = preg_women / 202, # number of villages in the data
    people_per_village = people_per_phc / villages_per_phc,
    preg_rate = preg_per_village / people_per_village,
    lives_saved_per_preg = 
      # From neonatal deaths
      effect_on_attended_births * effect_on_mortality * neonatal_mr * life_year_per_child_saved +
      # From supplementation
      effect_on_attended_births * effect_on_supplementation * neonatal_mr * life_year_per_child_saved,
    lives_saved_per_person = preg_rate * lives_saved_per_preg
  )

## Benefits --------------------------------------------------------------------

benefits <-
  reach %>%
  full_join(years) %>%
  arrange(year) %>%
  mutate(
    lives_saved = parameters_c$lives_saved_per_person * reach,
    benefits_usd = lives_saved * (gdp_pc %>% dplyr::filter(year == 2012) %>% pull(india)),
    benefits_op = lives_saved * global_parameters$daly_op / global_parameters$op_unit
  )

## Operating costs ------------------------------------------------------------

op_cost_c <-
  years %>%
  mutate(
    op_cost_usd = 0,
    div_year = year - 2010,
    op_cost_real = nominal_to_real(op_cost_usd, div_year),
    op_cost_op = 0
  )

# Save data --------------------------------------------------------------------

  DAM <-
  list(
    parameters = parameters_c,
    reach = reach,
    benefits = benefits,
    operating_costs = op_cost_c,
    innovation_costs = in_cost
  )

DAM %>%
  iwalk(
    ~ write_csv(
      .x, 
      here(
        "data",
        "DAM",
        "final",
        paste0(.y, ".csv")
      )
    )
  )

DAM %>%
  write_rds(
    here(
      "data",
      "DAM",
      "final",
      "all_data.rds"
    )
  )
