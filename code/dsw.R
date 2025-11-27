# Process Dispensers for Safe Water data

# Inputs -----------------------------------------------------------------------

## Years of interest -----------------------------------------------------------

years <-
  data.frame("year" = 2010:2035)

## Parameters ------------------------------------------------------------------

parameters <-
  read_csv(
    here(
      "data",
      "dsw",
      "raw",
      "parameters.csv"
    )
  ) %>% 
  select(-c(source, notes, parameter)) %>%
  pivot_longer(
    cols = c(kenya, malawi, uganda),
    names_to = "country"
  ) %>%
  pivot_wider(
    names_from = name
  )

## Reach -----------------------------------------------------------------------

reach <-
  read_csv(
    here(
      "data",
      "dsw",
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
      "dsw",
      "raw",
      "operating_cost.csv"
    )
  )

in_cost <-
  read_csv(
    here(
      "data",
      "dsw",
      "raw",
      "innovation_cost.csv"
    )
  )

# Calculations -----------------------------------------------------------------

## Parameters ------------------------------------------------------------------

parameters_c <-
  parameters %>%
  # Reduction in mortality in updated paper of Kremer et al (2023) is discounted by 25% to be conservative
  mutate(effect_on_mr = effect_on_mr * 0.75)

## Reach -----------------------------------------------------------------------

reach_c <-
  reach %>%
  rowwise() %>%
  mutate(
    dispensers = sum(c_across(c(kenya, uganda, malawi)))
  ) %>%
  ungroup

## Benefits --------------------------------------------------------------------

# Check takeup rates

benefits_by_country <-
  reach %>%
  pivot_longer(
    cols = -year,
    names_to = "country",
    values_to = "dispensers"
  ) %>%
  left_join(parameters) %>%
  mutate(
    # Deaths averted per dispenser
    births_per_dispenser_used = births_per_year * takeup,
    deaths_per_dispenser_used = births_per_dispenser_used * u5_mr,
    deaths_averted_per_dispenser_used = deaths_per_dispenser_used * effect_on_mr,
    years_per_dispenser_used = deaths_averted_per_dispenser_used * life_years_per_death,
    years = years_per_dispenser_used * dispensers
  ) %>%
  left_join(gdp_pc_long) %>%
  mutate(
    benefits_usd = years * gdp_pc,
    benefits_op = years * global_parameters$daly_op / global_parameters$op_unit
  )

benefits <-
  benefits_by_country %>%
  group_by(year) %>%
  summarise(
    across(
      starts_with("benefits"),
      ~ sum(., na.rm = TRUE)
    )
  )

## Operating costs ------------------------------------------------------------

op_cost_c <-
  reach_c %>%
  select(
    year,
    dispensers
  ) %>%
  ungroup %>%
  left_join(op_cost) %>%
  mutate(unit_cost = op_cost_usd / dispensers)
  
## Innovation costs ------------------------------------------------------------

in_cost_c <-
  in_cost %>%
  rowwise() %>%
  transmute(
    year,
    div_investment,
    nondiv_investment = sum(c_across(-c(div_investment, year)), na.rm = TRUE)
  ) %>%
  ungroup


# Save data --------------------------------------------------------------------

dsw <-
  list(
    parameters = parameters,
    reach = reach_c,
    benefits_per_country = benefits_by_country,
    benefits = benefits,
    operating_costs = op_cost_c,
    innovation_costs = in_cost_c
  )

dsw %>%
  iwalk(
    ~ write_csv(
      .x, 
      here(
        "data",
        "dsw",
        "final",
        paste0(.y, ".csv")
      )
    )
  )

dsw %>%
  write_rds(
    here(
      "data",
      "dsw",
      "final",
      "all_data.rds"
    )
  )
