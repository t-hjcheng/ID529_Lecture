# Functional Programming 
# ID529 

# Topics to cover 
# 
#  - sapply, apply family 
#  - purrr::map 
#  - summarize 
#  - stratified models
# 

# dependencies ------------------------------------------------------------

library(ID529data)
library(purrr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(tidycensus)


# simple example ----------------------------------------------------------



square <- function(x) {
  x^2
} 

c(square(1), square(2), square(3), square(4))

square(c(1,2,3,4))

sapply(c(1,2,3,4), square)


# modeling example with lm and gtsummary ----------------------------------

predictor_variables <- c(
  "age + sex_gender",
  "race_ethnicity + poverty_ratio",
  "days_dental_floss + PFAS_total",
  "total_energy")

formulas <- purrr::map_chr(1:4,
           ~ paste0("mean_BP ~ ",
                    paste(
                      predictor_variables[1:.],
                      collapse = " + ")
           ))

formulas <- purrr::map(formulas, formula)

models <- purrr::map(formulas, 
                     ~ lm(., data = nhanes_id529))

summary_tables <- purrr::map(models, gtsummary::tbl_regression)

gtsummary::tbl_merge(summary_tables)


# dplyr::summarize --------------------------------------------------------

nhanes_id529 |> 
  group_by(race_ethnicity, sex_gender,
           age_cat = cut(age, seq(10, 100, by = 10))) |> 
  summarize(
    mean_bp = mean(mean_BP, na.rm=TRUE),
    sd_BP = sd(mean_BP, na.rm=TRUE),
    mean_PFAS_total = mean(PFAS_total, na.rm=TRUE),
    sd_PFAS_total = sd(PFAS_total, na.rm=TRUE)) |> 
  View()

nhanes_summarized <- nhanes_id529 |> 
   group_by(race_ethnicity, sex_gender,
           age_cat = cut(age, seq(10, 100, by = 10))) |> 
   summarize(
     across(
     .cols = c(mean_BP, PFAS_total, starts_with("PF")),
     .fns = list(
       mean = ~ mean(., na.rm=TRUE),
       sd = ~ sd(., na.rm=TRUE),
       count_NAs = ~ sum(is.na(.)),
       n = ~ n()
     )))

nhanes_summarized <- nhanes_summarized |>
  mutate(PFAS_total_proportion_missing = PFAS_total_count_NAs / PFAS_total_n)

# trying to understand missingness
nhanes_summarized |> 
  arrange(desc(PFAS_total_proportion_missing)) |> 
  select(
    race_ethnicity, sex_gender, age_cat, 
    PFAS_total_proportion_missing,
    starts_with("PFAS_total_"),
    everything()) |> 
  View()


# stratified models -------------------------------------------------------

nested_data <- nhanes_id529 |> 
  nest_by(race_ethnicity)

# create linear models for each of the 
# racial/ethnic groups
nested_data <- nested_data |> mutate(
  model = list(
    lm(mean_BP ~ age, data = data)
  ))

nested_data <- nested_data |> mutate(
  coef_table = list(
    broom::tidy(model)))

nested_data |> 
  tidyr::unnest(cols = coef_table) |> 
  select(race_ethnicity, 
         term,
         estimate,
         p.value) |> 
  View()



# example of nest_by for maps  --------------------------------------------

library(tidycensus)

# get the county population sizes in a spatial
# data frame from the US Census American Community Survey
counties <- 
  tidycensus::get_acs(
    geography = 'county',
    year = 2020,
    variables = c(popsize = 'B01001_001'),
    geometry = TRUE)

counties <- counties |> 
  separate(col = 'NAME',
           sep = ', ',
           into = c('county', 'state'))

nested_counties <- 
  nest_by(counties, state)

nested_counties <- mutate(
  nested_counties,
  plot = list(
    ggplot(data, aes(fill = estimate)) +
      geom_sf(color = 'white', size = .2) +
      scale_fill_distiller(palette = 'Purples', 
                           labels = scales::comma_format(),
                           direction = 1) +
      ggtitle(paste0("County Populations in ", state))
  )
)

View(nested_counties)

nested_counties |> 
  filter(state == 'Massachusetts') |> 
  pull(plot)
