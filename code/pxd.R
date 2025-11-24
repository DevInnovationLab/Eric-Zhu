# Process gui2de data

# Inputs -----------------------------------------------------------------------

## Years of interest -----------------------------------------------------------

years <-
  data.frame("year" = 2010:2035)

## Parameters ------------------------------------------------------------------

parameters <-
  read_csv(
    here(
      "data",
      "pxd",
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
      "pxd",
      "raw",
      "reach.csv"
    )
  ) %>%
  remove_empty

## Cost ------------------------------------------------------------------

op_cost <-
  read_csv(
    here(
      "data",
      "pxd",
      "raw",
      "operating_cost.csv"
    )
  )

in_cost <-
  read_csv(
    here(
      "data",
      "pxd",
      "raw",
      "innovation_cost.csv"
    )
  )

# Calculations -----------------------------------------------------------------

## Parameters ------------------------------------------------------------------

parameters_c <-
  parameters %>%
  mutate(
    pct_increase_odisha = (effect_on_profit_odisha + baseline_profit_odisha) / baseline_profit_odisha,
    pct_increase_india = (effect_on_profit_india + baseline_profit_odisha) / baseline_profit_odisha,
    pct_increase_oaf = (effect_on_profit_oaf+ baseline_profit_oaf) / baseline_profit_oaf,
  )

## Benefits --------------------------------------------------------------------

benefits <-
  reach %>%
  full_join(years) %>%
  arrange(year) %>%
  mutate(
    odisha_usd = odisha * parameters$effect_on_profit_odisha,
    one_acre_fund_usd = one_acre_fund * parameters$effect_on_profit_oaf,
    india_usd = india * parameters$effect_on_profit_india,
    odisha_op = odisha * log(parameters_c$pct_increase_odisha) / global_parameters$op_unit,
    india_op = india * log(parameters_c$pct_increase_india) / global_parameters$op_unit,
    one_acre_fund_op = one_acre_fund * log(parameters_c$pct_increase_oaf) / global_parameters$op_unit
  ) %>%
  rowwise() %>%
  mutate(
    benefits_usd = sum(c_across(ends_with("usd")), na.rm = TRUE),
    benefits_op = sum(c_across(ends_with("op")), na.rm = TRUE)
  ) %>%
  ungroup 

## Operating costs ------------------------------------------------------------

# How is the cost in India defined?
op_cost_c <-
  map2_dfc(reach, op_cost, `*`) %>%
  mutate(year = sqrt(year)) %>%
  full_join(years) %>%
  arrange(year) %>%
  rename_with(
    ~ paste0("op_cost_usd_", .x),
    .cols = c(odisha, one_acre_fund, india)
  ) %>%
  left_join(
    reach %>% 
      rename_with(
        ~ paste0("reach_", .x),
        .cols = c(odisha, one_acre_fund, india)
      )
  ) %>%
  left_join(
    op_cost %>% 
      rename_with(
        ~ paste0("unit_cost_", .x),
        .cols = c(odisha, one_acre_fund, india)
      )
  ) %>%
  mutate(
    op_cost_op_odisha = nominal_cost_to_op(
      reach_odisha, unit_cost_odisha, parameters$baseline_profit_odisha
    ),
    op_cost_op_india = nominal_cost_to_op(
      reach_india, unit_cost_india, parameters$baseline_profit_odisha
    ),
    op_cost_op_oaf = nominal_cost_to_op(
      reach_one_acre_fund, unit_cost_one_acre_fund, parameters$baseline_profit_oaf
    ),
    div_year = year - 2010
  ) %>%
  rowwise() %>%
  mutate(
    op_cost_usd = sum(c_across(starts_with("op_cost_usd")), na.rm = TRUE),
    op_cost_op = sum(c_across(starts_with("op_cost_op")), na.rm = TRUE),
    op_cost_real = nominal_to_real(op_cost_usd, div_year)
  ) %>%
  ungroup

# Save data --------------------------------------------------------------------

pxd <-
  list(
    parameters = parameters_c,
    reach = reach,
    benefits = benefits,
    operating_costs = op_cost_c,
    innovation_costs = in_cost
  )

pxd %>%
  iwalk(
    ~ write_csv(
      .x, 
      here(
        "data",
        "pxd",
        "final",
        paste0(.y, ".csv")
      )
    )
  )

pxd %>%
  write_rds(
    here(
      "data",
      "pxd",
      "final",
      "all_data.rds"
    )
  )
