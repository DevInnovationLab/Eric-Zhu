# Process deworming data

# Read this paert of the paper carefully
pacman::p_load(
  tidyverse,
  here,
  janitor,
  zoo
)

# Inputs -----------------------------------------------------------------------

## Years of interest -----------------------------------------------------------

years <-
  data.frame("year" = 2010:2035)

## Parameters ------------------------------------------------------------------

parameters <-
  read_csv(
    here(
      "data",
      "deworming",
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
      "deworming",
      "raw",
      "reach.csv"
    )
  ) %>%
  remove_empty %>%
  select(-source)

## Burden ----------------------------------------------------------------------

burden <-
  read_csv(
    here(
      "data",
      "deworming",
      "raw",
      "burden.csv"
    )
  ) %>%
  select(-source)

## Cost ------------------------------------------------------------------------

in_cost <-
  read_csv(
    here(
      "data",
      "deworming",
      "raw",
      "innovation_costs.csv"
    )
  ) %>%
  select(-notes)

# Calculations -----------------------------------------------------------------

## Parameters ------------------------------------------------------------------

parameters_c <-
  parameters %>%
  # Present value of lifetime benefits from a year of deworming (USD)
  mutate(
    # Why divide GDP by 2?
    present_value_usd = (gdp_pc %>% dplyr::filter(year == 2023) %>% pull(india))/2 * 
      (exp(parameters$present_value) - 1)
  )

## Reach -----------------------------------------------------------------------

# We want estimates of reach from 2015 to 2035
reach_c <-
  reach %>%
  full_join(years) %>%
  arrange(year) %>%
  # If there is no data for a year, we use the same as the previous year
  fill(round1, .direction = "down")

## Worm burden -----------------------------------------------------------------

# Adjust for worm burden
burden_c <-
  burden %>%
  full_join(years) %>%
  arrange(year) %>%
  group_by(year) %>%
  summarise(burden = mean(burden)) %>%
  mutate(
    lag = burden,
    linear = na.approx(burden, na.rm = FALSE),
    carryforward = burden
  ) %>%
  fill(lag, .direction = "up") %>%
  fill(carryforward, .direction = "down") %>%
  transmute(
    year,
    burden_final = case_when(
      !is.na(burden) ~ burden,
      year == 2015 ~ lag,
      year > 2020 ~ carryforward,
      TRUE ~ linear
    ),
    burden_final = burden_final/100
  )

## Benefits --------------------------------------------------------------------

# These benefits are calculated differently from other interventions

benefits <-
  reach_c %>%
  rowwise() %>%
  left_join(
    burden_c %>%
      select(year, burden_final)
  ) %>%
  mutate(
    # Final reach is defined as the minimum of two rowns
    min = min(round1, round2, na.rm = TRUE),
    # Adjust for take up 
    takeup_adj = min * parameters$takeup,
    # Adjust for worm burden
    burden_adj = takeup_adj * burden_final,
    # Calculate  benefits
    benefits_usd = burden_adj * parameters_c$present_value_usd,
    benefits_op = burden_adj * parameters_c$present_value / global_parameters$op_unit
  )

## Operating costs ------------------------------------------------------------

op_costs <-
  reach_c %>%
  rowwise() %>%
  mutate(
    # Assume full yearly cost of all kids reached by NDD campaign at least once in the year
    reach = max(round1, round2, na.rm = TRUE),
    unit_cost = parameters$unit_cost,
    op_cost_usd = reach * unit_cost
  )

## Innovation costs ------------------------------------------------------------

in_cost_c <-
  in_cost %>%
  rowwise() %>%
  transmute(
    year,
    div_investment,
    nondiv_investment = sum(c_across(-c(year, div_investment)), na.rm = TRUE)
  )

# Save data --------------------------------------------------------------------

deworming <-
  list(
    parameters = parameters,
    reach = reach_c,
    benefits = benefits,
    operating_costs = op_costs,
    innovation_costs = in_cost_c
  )

deworming %>%
  iwalk(
    ~ write_csv(
      .x, 
      here(
        "data",
        "deworming",
        "final",
        paste0(.y, ".csv")
      )
    )
  )

deworming %>%
  write_rds(
    here(
      "data",
      "deworming",
      "final",
      "all_data.rds"
    )
  )
