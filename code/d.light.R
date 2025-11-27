# d.light

# Inputs -----------------------------------------------------------------------

## Years of interest -----------------------------------------------------------

years <-
  data.frame("year" = 2010:2035)

## Parameters ------------------------------------------------------------------

parameters <-
  read_csv(
    here(
      "data",
      "dlight",
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
      "dlight",
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
      "dlight",
      "raw",
      "operating_cost.csv"
    )
  ) %>%
  remove_empty %>%
  select(year, unit_cost)

in_cost <-
  read_csv(
    here(
      "data",
      "dlight",
      "raw",
      "innovation_cost.csv"
    )
  ) %>%
  remove_empty

# Calculations -----------------------------------------------------------------

## Benefits --------------------------------------------------------------------

benefits <-
  reach %>%
  mutate(
    sales_year = sales_cumulative - lag(sales_cumulative),
    within_lifespan = slide_dbl(
      sales_year, 
      ~ sum(., na.rm = TRUE), 
      .before = parameters$lifespan - 1, 
      .after = 0, 
      .complete = FALSE
    ),
    # why are they adding the expenditure in the excel file?
    benefits_usd = (within_lifespan - sales_year) * parameters$annual_savings
  )
  
## Operating costs -------------------------------------------------------------

op_cost_c <-
  op_cost %>%
  full_join(years) %>%
  arrange(year) %>%
  fill(unit_cost, .direction = "down") %>%
  left_join(
    benefits %>% 
      select(year, sales_year)
  ) %>%
  mutate(
    div_year = year - 2010,
    op_cost_usd = unit_cost * sales_year,
    op_cost_real = nominal_to_real(op_cost_usd, div_year)
  )

# Save data --------------------------------------------------------------------

dlight <-
  list(
    parameters = parameters,
    reach = reach,
    benefits = benefits,
    operating_costs = op_cost_c,
    innovation_costs = in_cost
  )

dlight %>%
  iwalk(
    ~ write_csv(
      .x, 
      here(
        "data",
        "dlight",
        "final",
        paste0(.y, ".csv")
      )
    )
  )

dlight %>%
  write_rds(
    here(
      "data",
      "dlight",
      "final",
      "all_data.rds"
    )
  )
