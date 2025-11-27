#==============================================================================#
#                       Development Innovation Lab                             #
#                         Template main script                                 #
#==============================================================================#


pacman::p_load(
  tidyverse,
  here,
  janitor,
  zoo,
  slider
)

# Global inputs ----------------------------------------------------------------

global_parameters <-
  read_csv(
    here(
      "data",
      "global",
      "parameters.csv"
    )
  ) %>% 
  select(value, name) %>%
  pivot_wider

gdp_pc <-
  read_csv(
    here(
      "data",
      "global",
      "gdp_pc.csv"
    )
  )

gdp_pc_long <-
  gdp_pc %>%
  pivot_longer(
    cols = -year,
    values_to = "gdp_pc",
    names_to = "country"
  )


# Functions --------------------------------------------------------------------

nominal_cost_to_op <- function(reach, unit_cost, gdp_pc) {
  reach * log(1 - unit_cost/gdp_pc)/global_parameters$op_unit
}

nominal_to_real <- function(nominal, year) {
  nominal / ((1 + global_parameters$discount_rate) ^ year)
}

innovation_costs <- function(data) {
  data %>%
    full_join(years) %>%
    arrange(year) %>%
    mutate(
      div_year = year - 2010
    ) %>%
    mutate(
      across(
        contains("investment"),
        ~ replace_na(., 0)
      ),
      across(
        contains("investment"),
        ~ . / ((1 + global_parameters$discount_rate) ^ div_year),
        .names = "{.col}_real"
      ),
      across(
        contains("investment"),
        ~ cumsum(.),
        .names = "{.col}_todate"
      ),
      total_investment_todate = div_investment_todate + nondiv_investment_todate,
      total_investment_real_todate = div_investment_real_todate + nondiv_investment_real_todate,
      div_share = div_investment_real_todate/total_investment_real_todate
    )
}

net_benefits_inputs <- function(benefits_data, op_cost_data, in_cost_data) {
  in_cost_data %>%
    left_join(op_cost_data) %>%
    left_join(benefits_data) %>%
    select(
      year, div_year, 
      benefits_usd, op_cost_usd, 
      div_share, div_investment_real_todate, 
      benefits_op, op_cost_op
    ) %>%
    mutate(
      net_benefits_usd = benefits_usd - op_cost_usd,
      net_benefits_op = benefits_op + op_cost_op,
      across(
        starts_with("net_benefits"),
        ~ . * div_share,
        .names = "attributed_{.col}"
      ),
      across(
        starts_with("attributed_"),
        ~ nominal_to_real(., div_year),
        .names = "real_{.col}"
      ),
      across(
        starts_with("real_attributed"),
        ~ cumsum(.),
        .names = "{.col}_todate"
      ),
      across(
        starts_with("real_attributed"),
        ~ cumsum(.),
        .names = "{.col}_todate"
      ),
      across(
        ends_with("_todate"),
        ~ . / (div_investment_real_todate * 1.12),
        .names = "{.col}_todate"
      )
    )
}


#==============================================================================#