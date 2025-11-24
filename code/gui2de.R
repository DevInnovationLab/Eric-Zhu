# Process gui2de data

pacman::p_load(
  tidyverse,
  here,
  janitor,
  zoo
)

# Inputs -----------------------------------------------------------------------

## Years of interest -----------------------------------------------------------

years <-
  data.frame("year" = 2010:2024)

## Parameters ------------------------------------------------------------------

parameters <-
  read_csv(
    here(
      "data",
      "gui2de",
      "raw",
      "parameters.csv"
    )
  ) %>% 
  select(value, name) %>%
  pivot_wider %>%
  mutate(
    injuries_per_accident = deaths_per_accident * injuries_per_death,
    dalys_saved_per_vehicle = accidents_reduction * deaths_per_accident * years_lost_per_death +
      accidents_reduction * injuries_per_accident * disability_years_per_accident
  )

## Reach -----------------------------------------------------------------------

reach <-
  read_csv(
    here(
      "data",
      "gui2de",
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
      "gui2de",
      "raw",
      "operating_cost.csv"
    )
  )

in_cost <-
  read_csv(
    here(
      "data",
      "gui2de",
      "raw",
      "innovation_cost.csv"
    )
  )

# Calculations -----------------------------------------------------------------

## Calculated parameters -------------------------------------------------------

parameters <-
  parameters %>%
  mutate(
    injuries_per_accident = deaths_per_accident * injuries_per_death,
    dalys_saved_per_vehicle = 
      # Deaths
      accidents_reduction * deaths_per_accident * years_lost_per_death +
      # Injuries
      accidents_reduction * injuries_per_accident * disability_years_per_accident
  )

## Impute reach ----------------------------------------------------------------

reach_c <-
  reach %>%
  full_join(years) %>%
  arrange(year) %>%
  mutate(
    vehicles = case_when(
      year %in% c(2010, 2023, 2024) ~ replace_na(vehicles, 0),
      TRUE ~ vehicles
    )
  ) %>%
  fill(vehicles, .direction = "down")

## Benefits --------------------------------------------------------------------

benefits <-
  reach_c %>%
  left_join(
    gdp_pc %>%
      select(year, gdp_pc = kenya)
  ) %>%
  mutate(
    div_year = year - 2010,
    dalys_saved = vehicles * parameters$dalys_saved_per_vehicle,
    benefits_usd = dalys_saved * gdp_pc,
    benefits_op = dalys_saved * global_parameters$daly_op / global_parameters$op_unit
  )

## Operating costs ------------------------------------------------------------

op_cost_c <-
  op_cost %>%
  left_join(reach_c) %>%
  mutate(
    op_cost_usd = op_cost,
    div_year = year - 2010,
    unit_cost = op_cost / vehicles,
    op_cost_real = nominal_to_real(op_cost_usd, div_year),
    op_cost_op = nominal_cost_to_op(
      reach = vehicles, 
      unit_cost, 
      gdp_pc %>% dplyr::filter(year == 2010) %>% pull(india)
    )
  )

# Save data --------------------------------------------------------------------

gui2de <-
  list(
    parameters = parameters,
    reach = reach_c,
    benefits = benefits,
    operating_costs = op_cost_c,
    innovation_costs = in_cost
  )

gui2de %>%
  iwalk(
    ~ write_csv(
      .x, 
      here(
        "data",
        "gui2de",
        "final",
        paste0(.y, ".csv")
      )
    )
  )

gui2de %>%
  write_rds(
    here(
      "data",
      "gui2de",
      "final",
      "all_data.rds"
    )
  )
