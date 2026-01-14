# Name:   id529-day3-messyData.R
# Author: Jarvis Chen
# The is follow-along code to accompany the activity on Day 3 of ID529
# Data cleaning with messy data
# Updated: 14 January 2026

# Dependencies and setup ------------------------------------------------------------

library(tidyverse)
library(scales)

# Read in the messy data from csv
messy_data <- read_csv("data/Day 3/messy_data.csv")

#Have a look
View(messy_data)
glimpse(messy_data)
head(messy_data)


# Clean column names ------------------------------------------------------

# We can use the clean_names() function from the janitor package to clean up the column names
df <- messy_data |>
  janitor::clean_names()


# Explore duplicates ------------------------------------------------------

# there appear to be some ID numbers that appear multiple times
table(df$id_number)

# We can sort by id_number to see if there is a pattern
df <- df |> arrange(id_number)
View(df)


# Standardize missing value codes -----------------------------------------

# There appear to be non-standard missing codes in the dataset that we need to clean up.
# Note that self_identified_gender and highest_education_completed are character variable
# and we need to change unknown to NA_character_
# We also see that the numeric variables age_at_exam, hours_of_sleep_per_night, and
# household_income_before_taxes have -99, -77, and -999 that need to be changed to
# NA_real_
# We will take advantage of the across() function to apply mutate to multiple columns at once

df <- df |>
  mutate(
    across(c(hispanic_ethnicity, self_identified_gender, highest_education_completed),
           ~ na_if(.x, "unknown")),
    across(c(age_at_exam, hours_of_sleep_per_night, household_income_before_taxes),
           ~ replace(.x, .x < 0, NA_real_)))


# Remove duplicates -------------------------------------------------------

# Now we can count the number of missing variables in order to identify duplicate records
# that should be discarded
unique_df <- df |>
  # create a new variable that counts the number of missing variables across this row
  mutate(missing_count = rowSums(is.na(across(everything())))) |>
  # sort each id_number's observations by the number of missing columns (from fewest to most)
  arrange(id_number, missing_count) |>
  # group by id_number
  group_by(id_number) |>
  # keep the row with the minimum missing_count for each id_number
  # with_ties = FALSE ensures that we get exactly one row per id_number
  # even if two rows tie
  slice_min(missing_count, n = 1, with_ties = FALSE) |>
  # it's often a good idea to ungroup() after using a group_by
  ungroup() |>
  # drop the missing_count variable
  select(-missing_count)


# Collapsing categories ---------------------------------------------------


# We would like to do some collapsing of these variables:
# 1. Create a combined race/ethnicity variable from race_self_identified and hispanic_ethnicity
# This new variable should combine Asian and Native Hawaiian and Pacific Islander individuals into
# the same category. 
# 2. Create a collapsed gender variable with categories cisgender female, cisgender male, and trans/non-binary.
# 3. Create a collapsed education variable with categories less than high school, high school graduate, and college graduate.

table(unique_df$race_self_identified, unique_df$hispanic_ethnicity, useNA="ifany")


unique_df <- unique_df |>
  mutate(raceth = factor(
    case_when(
      race_self_identified == "White" &
        hispanic_ethnicity == "Non-Hispanic" ~ "Non-Hispanic White",
      race_self_identified == "Black" &
        hispanic_ethnicity == "Non-Hispanic" ~ "Non-Hispanic Black",
      hispanic_ethnicity == "Hispanic" ~ "Hispanic",
      is.na(race_self_identified) |
        is.na(hispanic_ethnicity) ~ NA_character_,
      TRUE ~ "underrepresented racial/ethnic group"
    ),
    levels = c(
      "Non-Hispanic White",
      "Non-Hispanic Black",
      "Hispanic",
      "underrepresented racial/ethnic group"
    )
  ))


unique_df <- unique_df |>
  mutate(
    gender_collapsed = if_else(
      self_identified_gender %in% c(
        "transgender female",
        "transgender male",
        "non-binary or genderqueer"
      ),
      "trans/non-binary",
      self_identified_gender
    ))

table(unique_df$gender_collapsed)


# Can you write code to create a collapsed variable for education?
unique_df <- unique_df |>
  mutate(
    education_collapsed = case_when(
      
      
      
    )
  )

table(unique_df$education_collapsed)

# Create categorical variables from continuous variables ------------------

# We would like to create a dichotomous sleep variable where 0 = >=7 hours of sleep per night and 1= <7 hours of sleep per night
# We would also like to create a categorical income variable with categories <$35,000, $35000-$50000, $50000-$100000, and >=$100000
# We would also like to create an income quintile variable


# How should we create a dichotomous sleep variable here?
unique_df <- unique_df |>
  mutate(insufficient_sleep = )

table(unique_df$insufficient_sleep)

unique_df <- unique_df |>
  mutate(income_cat = cut(household_income_before_taxes, 
                          breaks = c(0, 25000, 50000, 75000, 100000, Inf),
                          right = FALSE, 
                          include.lowest=TRUE,
                          labels = c("<$25k", "$25–49k", "$50–74k", "$75–99k", "$100k+")),
         income_quint = ntile(household_income_before_taxes, 5))


# Let's visualize how income_cat has split up the household_income_before_taxes variable
ggplot(unique_df, 
       aes(x=income_cat, y=household_income_before_taxes)) + 
  geom_boxplot() +
  scale_y_continuous(labels = label_dollar(accuracy = 1)) +
  theme_minimal()

# Let's compare how income_quint has split up the household_income_before_taxes variable
ggplot(unique_df, 
       aes(x=factor(income_quint), y=household_income_before_taxes)) + 
  geom_boxplot() +
  scale_y_continuous(labels = label_dollar(accuracy = 1)) +
  theme_minimal()


# Use group_by and summarize ----------------------------------------------


# to see the minimum and maximum of the quintile categories created by ntile()
# we can use group_by and summarize
unique_df |>
  group_by(income_quint) |>
  summarise(mininc = min(household_income_before_taxes, na.rm=TRUE),
            maxinc = max(household_income_before_taxes, na.rm=TRUE))


# suppose that we want to calculate the proportion of subjects
# with insufficient sleep in each gender and racial/ethnic group

proportions_by_gender_raceth <- unique_df |>
  # group by gender, race/ethnicity, and insufficient sleep category
  group_by(gender_collapsed, raceth, insufficient_sleep) |>
  # count the number of subjects in each category
  # Note the use of .groups='drop' which removes the group_by after summarizing
  summarise(count_insufficient_sleep = n(), .groups='drop') |>
  # Now group by gender and race/ethnicity
  group_by(gender_collapsed, raceth) |>
  # Calculate the proportion
  mutate(proportion_insufficient_sleep = count_insufficient_sleep/sum(count_insufficient_sleep, na.rm=T)) |>
  # Remove the grouping
  ungroup()

head(proportions_by_gender_raceth)
