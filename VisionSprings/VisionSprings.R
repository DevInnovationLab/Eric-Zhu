library(dplyr)
library(data.table)

folder_path <- "/users/junrongzhu/Documents/GitHub/DIV" #change to the path to your folder containing the DIV folder
input <- file.path(folder_path, "VisionSprings")

#section 0: Global inputs
global_input_gdp <- read.csv(file.path(input, "global_input_gdp.csv"))
discount_rate <- 0.04
OPunit <- 0.0000199998
India2010GDPpc <- 1347.5

#section 1: reach
reach <- read.csv(file.path(input, "Reach.csv")) #reach is treated as parameter (As annotated, it requires updating)

#section 2: benefits ----------------------------------------------------
#parameter: Productivity gain from glasses use (22% cut in half for conservative estimate)
prod_gain <- 0.11

#calculation 
template <- copy(reach)
benefits_2 <- global_input_gdp %>%
  select(
    Nominal.GDP.per.capita..USD...World.Bank.,
    LIC.agriculture.value.added.per.worker..nominal.USD.
  ) %>%
  mutate(
    Year = Nominal.GDP.per.capita..USD...World.Bank.
  ) %>%
  select(
    -Nominal.GDP.per.capita..USD...World.Bank.
  )

benefits_3 <- merge(template, benefits_2, by = "Year")

benefits_final <- benefits_3 %>% mutate(
  Glasses.in.circulation = as.numeric(gsub(",", "", Glasses.in.circulation)),
  Nominal_benefits_usd = Glasses.in.circulation * prod_gain * LIC.agriculture.value.added.per.worker..nominal.USD. ,
  Real_benefits_2010usd = Nominal_benefits_usd/((1+discount_rate)^DIV.Year),
  Nominal_benefits_OP = Glasses.in.circulation * log(1+prod_gain) / OPunit
) %>% select(-LIC.agriculture.value.added.per.worker..nominal.USD.)

#section 3: Operating Costs ----------------------------------------------------
#parameter 1: Per distributed cost (from Reddy et. al 2018)
per_dist_cost <- 10.2

#calculation
Operating_cost_final <- template %>% mutate(
  Glasses.distributed = as.numeric(gsub(",", "", Glasses.distributed)),
  Nominal_op_cost_usd = Glasses.distributed * per_dist_cost , 
  Real_op_cost_2010usd = Nominal_op_cost_usd /((1+discount_rate)^DIV.Year),
  Nominal_op_cost_OP = Glasses.distributed * log(1-(per_dist_cost/India2010GDPpc))/OPunit
)

#section 4: Innovation Costs ----------------------------------------------------
#note: Assumption: 2010-2011 identical to 2012 for non-DIV funding. Likely overstates funding and understates DIV's share.                                        
#Input 1: List of Non-DIV VisionSpring grants
non_div_grant <- read.csv(file.path(input, "list_of_nonDIV_grants.csv")) %>% mutate(
  Skoll..Nominal. = as.numeric(gsub("[$,]", "", Skoll..Nominal.)),
  Skoll..Nominal. = ifelse(is.na(Skoll..Nominal.), 0, Skoll..Nominal.),
  Mulago..Nominal. = as.numeric(gsub("[$,]", "", Mulago..Nominal.)),
  GCC..Nominal. = as.numeric(gsub("[$,]", "", GCC..Nominal.)),
  Peery..Nominal. = as.numeric(gsub("[$,]", "", Peery..Nominal.))
)

div_investment <- read.csv(file.path(input, "div_grant.csv")) %>% mutate(
  Nominal.DIV.investment..USD. = as.numeric(gsub("[$,]", "", Nominal.DIV.investment..USD.)),
  Nominal.DIV.investment..USD. = ifelse(is.na(Nominal.DIV.investment..USD.), 0 , Nominal.DIV.investment..USD.)
)

#calculations 
non_div_grant <- non_div_grant %>% mutate(
  Nominal_total = Skoll..Nominal. + Mulago..Nominal.+ GCC..Nominal. + Peery..Nominal. ,
  DIV.Year = Year - 2010, 
  Real_total = Nominal_total/(1+discount_rate)^(DIV.Year)
)

Innovation_costs_final <- merge(template, div_investment, by = "Year") %>%
  select(-Glasses.distributed, -Glasses.in.circulation, -DIV.Year.y) %>%
  mutate(
    DIV.Year = DIV.Year.x,
    Real.DIV.Investment =
      Nominal.DIV.investment..USD. / (1 + discount_rate)^DIV.Year
  ) %>%
  arrange(DIV.Year) %>%  
  mutate(
    Nominal.DIV.todate = cumsum(Nominal.DIV.investment..USD.),
    Real.DIV.todate    = cumsum(Real.DIV.Investment)
  ) %>%
  select(-DIV.Year.x)


Innovation_costs_final <- merge(
  Innovation_costs_final,
  non_div_grant,
  by = "DIV.Year",
  all.x = TRUE
) %>%
  select(
    -Year.y,
    -Skoll..Nominal.,
    -Mulago..Nominal.,
    -GCC..Nominal.,
    -Peery..Nominal.
  ) %>% rename(Nominal_nonDIV_total =Nominal_total , Real_nonDIV_total = Real_total) %>% mutate(
    Nominal_nonDIV_total= ifelse(is.na(Nominal_nonDIV_total), 0, Nominal_nonDIV_total),
    Real_nonDIV_total = ifelse(is.na(Real_nonDIV_total),0,Real_nonDIV_total) ,
    Nominal.nonDIV.todate = cumsum(Nominal_nonDIV_total),
    Real.nonDIV.todate    = cumsum(Real_nonDIV_total),
    DIV_share = Real.DIV.todate/(Real.DIV.todate + Real.nonDIV.todate)
  ) 








