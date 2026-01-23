# Let's Review

# We circulated a survey about what would be the most useful material for us to
# review and revisit, as well as if there are any questions that would be useful
# for us to go over together.

# Overall topics to try to cover: 
# 
#   - dplyr and data manipulation 
#   - color in ggplot2 
#   - pseudocoding
#   - time since last recession
#   - logistic regression
#   - chi squared test

# dependencies ------------------------------------------------------------

library(ID529data)
library(tidyverse)
library(gtsummary)
library(magrittr)

# dplyr and data manipulation ---------------------------------------------

# Most requested topic: dplyr and data manipulation 
# 
# Remember you can load dplyr with either library(dplyr) or just load the 
# whole tidyverse like I've done using library(tidyverse)
# 
# Let's think of what are some of the most important tasks using dplyr and data 
# manipulation: 
# 
#   - Making a new column out of information from other columns
#     - Cleaning a single column 
#     - case_when 
#     - if_else 
#   - Creating summaries
#   - Filtering data 
#   - Joining data
# 
# Remember you can access the dplyr cheat sheet here: 
# https://www.rstudio.com/wp-content/uploads/2015/02/data-wrangling-cheatsheet.pdf 

View(nhanes_id529) # typically I don't write View() into a script and only run it on the console

# dplyr::mutate 
# 
# Cleaning a single column: 
# Sometimes we want to create a TRUE/FALSE variable out of a numeric variable:

nhanes_id529 |> 
  mutate(age_over_45 = age > 45) |> 
  select(id, age, age_over_45) |> 
  View()

# Or clean string text using functions from the stringr package: 

nhanes_id529 |> 
  mutate(hispanic = stringr::str_detect(race_ethnicity, pattern = "^Hispanic")) |> 
  select(id, race_ethnicity, hispanic) |> 
  View()
  
# Using case_when in a mutate: 
# If we want to create multiple levels to the new column beyond just TRUE/FALSE, 
# we could use a case_when: 

nhanes_id529 |> 
  mutate(age_category = 
           case_when(
             age < 25 ~ "Age: [12, 25)",
             age < 50 ~ "Age: [25, 50)",
             age < 75 ~ "Age: [50, 75)",
             age >= 75 ~ "Age: [75, 80]")) |>
  select(id, age, age_category) |> 
  View()

# Now is a good time to mention that if I wanted to make any of these updates 
# to the nhanes_id529 object, I'd have to write them back to the object like so:

nhanes_id529 <- nhanes_id529 |> 
  mutate(age_category = 
           case_when(
             age < 25 ~ "Age: [12, 25)",
             age < 50 ~ "Age: [25, 50)",
             age < 75 ~ "Age: [50, 75)",
             age >= 75 ~ "Age: [75, 80]"))

# if_else takes 3 arguments: 
#   - A conditional value (can be a vector) (TRUE/FALSE)
#   - A return value for when the conditional value is TRUE (can be a vector)
#   - A return value for when the conditional value is FALSE (also can be a vector). 
# 
# dplyr::if_else has the following advantages over the ifelse in base R: 
#   - if_else checks that the two return values have the same type 
#   - it has a 'missing' argument for handling NA values 
#   - it doesn't automatically convert Dates to numeric type 
# 
# here's two examples, one not vectorized and one vectorized: 
if_else(15 > 5, 'yes', 'no')

if_else(c(TRUE, FALSE, TRUE), 
        c('yes', 'affirmative', 'si'), 
        c('no', 'negative', 'incorrect'))

# when do we use if_else in a mutate? 
# 
# suppose we were trying to improve a plot by controlling when we plot a label 
# for each point. 
# 
# what we might do is construct a column with an indicator for whether or not 
# to plot the label and then use an if_else to construct the string for the 
# label to be plotted. 

df <- data.frame(
  x = c(1,2,3,4),
  y = c(5,6,7,8),
  lgl_plot_label = c(TRUE, FALSE, FALSE, TRUE),
  name = c('observation 1', 'observation 2', 'observation 3', 'observation 4'))

# suppose I had already created the indicator column lgl_plot_label: 
# 
# now I can use if_else to create the label for some points:
df <- df |> mutate(
  label = if_else(lgl_plot_label,
                  # create the string "(x,y)" rounded to 0 digits past the decimal
                  name,
                  ''
                  ))

ggplot(df, aes(x, y, label = label)) + 
  geom_point() + 
  geom_text(mapping = aes(x = x+.25))


# creating summaries ------------------------------------------------------

# one of the most useful functions in dplyr is summarize. it let's you 
# condense a lot of observations down into summary statistics. 

# you can actually use summarize without group_by to just get overall summary 
# statistics on whatever columns you want: 

# for example, to get the mean age in a dataset:
nhanes_id529 |> summarize(mean_age = mean(age))

# or blood pressure across an entire dataset:
# remember, mean_BP is the individual's mean of probably 3 blood pressure
# measurements
nhanes_id529 |> summarize(mean_bp = mean(mean_BP, na.rm = TRUE))

# but if I want to get those summary statistics broken down by 
# some other category, then I want to use group_by() |> summarize() 

nhanes_id529 |> 
  group_by(age_category) |> 
  summarize(mean_bp = mean(mean_BP, na.rm=TRUE))

# I can use multiple categories to group_by if I want to:

nhanes_id529 |> 
  group_by(sex_gender, age_category) |> 
  summarize(mean_bp = mean(mean_BP, na.rm=TRUE))

# I can also have multiple summary statistics

nhanes_id529 |> 
  group_by(sex_gender, age_category) |>
  summarize(
    count = n(), # n() counts the number of observations / rows in the category 
    mean_bp = mean(mean_BP, na.rm = TRUE),
    mean_PFAS_total = mean(PFAS_total, na.rm=TRUE), # sd calculates the standard deviation
    sd_bp = sd(mean_BP, na.rm = TRUE),
    sd_PFAS_total = sd(PFAS_total, na.rm = TRUE))


# filtering data ----------------------------------------------------------

# sometimes we want to filter our data, like if we ran a survey-based study
# and wanted to impose strict study-inclusion criteria, like we only wanted
# to study those aged 25-74. 
# 
# we can use a filter to restrict our data 

nhanes_id529 |> 
  filter(age >= 25, age <= 74) |> 
  View()

# we could overwrite the data in our R session if we wanted to with something
# like this:
# 
# nhanes_id529 <- nhanes_id529 |> 
#   filter(age > 25, age <= 74) |> 
#     head()
nhanes_id529 %<>% filter(age >= 25, age <= 74)



# left join  --------------------------------------------------------------

new_labels <- data.frame(
  age_category = c("Age: [12, 25)",
  "Age: [25, 50)",
  "Age: [50, 75)",
  "Age: [75, 80]"),
  new_label = c('Young Adult', 'Adult', 'Older', 'Oldest'))


nhanes_id529 <- nhanes_id529 |> 
  left_join(new_labels, by = c('age_category' = 'age_category')) |> 
  select(id, age, age_category, new_label) |> 
  View()


# color in ggplot2 --------------------------------------------------------

ggplot(nhanes_id529, aes(x = age, y = mean_BP, 
                         color = sex_gender)) + 
  geom_point(alpha = .5) + 
  scale_color_manual(
    values = c(
      'Female' = '#00b894', 
      'Male' = '#fd79a8'
    )
  )

nhanes_id529 |> 
  filter(PFAS_total < 200) |> 
  ggplot(
       aes(x = mean_BP,
           y = PFAS_total,
           color = age)) + 
  geom_point() + 
  scale_color_distiller(
    palette = 'BrBG',
    direction = 1)

# pseudocoding ------------------------------------------------------------

# pseudocode for the COVID OSHA project 
# 
# https://github.com/ctesta01/covid_osha 
# 
# 0. dependencies 
# 
# 1. load_data
#     covid <- load_covid_data()
#     osha <- load_osha_data()
# 
# 2. clean data 
#     covid <- clean_covid(covid)
#     osha <- clean_osha(osha)
# 
# 3. draft function for regional plot 
# 
# create_regional_plot <- function(
#     region,
#     covid,
#     osha
#     ) {
#   inset_plot <- create_inset_map(region)
#   
#   # ggplot with two axes 
#   osha_and_covid_plot <- ggplot() + 
#     geom_bar(osha, ...) +  # stacked bar plot by industry 
#     geom_line() + 
#     scale_y_continuous( ... # configure for a second y axis)
# 
#   # combine osha and covid plot with the inset 
#   osha_and_covid_plot_w_inset <- ... 
# } 
# 
# 4. create 5-panel figure 
#     cowplot::plot_grid( # not sure how to do layout yet 
#       create_regional_plot('national', covid, osha),
#       create_regional_plot('midwest', covid, osha),
#       create_regional_plot('south', covid, osha),
#       create_regional_plot('northeast', covid, osha),
#       create_regional_plot('west', covid, osha)
#  )
#      

# time since last recession -----------------------------------------------

# get time since the most recent recession 

recessions <- c(
  1929,
  1937,
  1945,
  1949,
  1953,
  1958,
  1960,
  1969,
  1973,
  1980,
  1981,
  1990,
  2000,
  2008)

observation_years <- c(
  2011, 
  2021, 
  2005,
  2001,
  2004,
  2005)

time_since_most_recent_recession <- function(
    recessions, 
    observation_years) {
  
  purrr::map_dbl(observation_years,
                 ~ . - max(recessions[recessions <= .]))
}

# example working through intermediate steps:
purrr::map(observation_years,
               ~ . - max(recessions[recessions <= .]))

time_since_most_recent_recession(
  recessions,
  observation_years)


# logistic regression -----------------------------------------------------

# let's do a logistic regression on stage two or higher elevated
# blood pressure 

nhanes_id529 <- nhanes_id529 |> 
  mutate(high_blood_pressure = mean_BP >= 140)

logistic_model <- glm(
  high_blood_pressure ~ age + sex_gender + 
    race_ethnicity, 
  family = binomial(link = 'logit'),
  data = nhanes_id529)

gtsummary::tbl_regression(logistic_model, exponentiate = TRUE)

vip::vip(logistic_model)

# chi squared test --------------------------------------------------------

observations <- matrix(
  c(134, 155, # treated (success, failure)
    133, 195), # untreated (success, failure)
  byrow = TRUE,
  ncol = 2)

colnames(observations) <- c('success', 'failure')
rownames(observations) <- c('treated', 'untreated')

observations

chisq.test(observations)
