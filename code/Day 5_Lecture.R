# 5.1 ---------------------------------------------------------------------
install.packages('tigris')

library(tigris)
library(ggplot2)

manhattan_roads <- roads("NY", "New York")

ggplot(manhattan_roads) + 
  geom_sf() + 
  theme_void()


library(tidyverse)
library(sf)

# you might have some data in a shapefile -- 
# data/shapefile_unzipped/
#  -- cb_2013_us_county_20m.dbf
#  -- cb_2013_us_county_20m.prj
#  -- cb_2013_us_county_20m.shp
#  -- cb_2013_us_county_20m.shp.iso.xml
#  -- cb_2013_us_county_20m.shp.xml
#  -- cb_2013_us_county_20m.shx
#  -- county_20m.ea.iso.xml

counties <- read_sf(here("data/shapefile_unzipped"))


# 5.2 ---------------------------------------------------------------------
#na


# 5.3 ---------------------------------------------------------------------
#see Day 5 id529_day4_regression_models.R


# 5.4 ---------------------------------------------------------------------
# ############################################################################ #
# COURSE :  ID 529: Data Management and Analytic Workflows in R
# ACTIVITY: Working with joins
# CREATED:  01/13/2023
# INPUTS:   ID 529 NHANES dataset (ID529data::nhanes_id529) 
# OUTPUTS:  N/A
# AUTHOR:   Dean Marengi
# NOTES:    ID 529 dataset details: https://github.com/ID529/ID529data
# ############################################################################ #

# PACKAGES ---------------------------------------------------------------------
# Uncomment and run lines 15-18  (if you have not already 
# loaded the ID529 NHANES data or installed the tidyverse)

# install.packages("tidyverse")
# install.packages("devtools")
# library(devtools)
# devtools::install_github("ID529/ID529data")

# Load relevant package libraries
library(ID529data) # Includes ID 529 NHANES data
library(tidyverse) # Includes dplyr

# Load ID 529 NHANES data 
data(nhanes_id529, package = 'ID529data')

#!!!直接load ID529_Lecture\data\Day 5 裡面的nhanes_id529就好!!!!

# SPLIT UP THE DATASET ---------------------------------------------------------
# Run set.seed(123) to ensure the random sample we take is the same
set.seed(123)

# Dataset 1: Demographic and clinical parameters
clinical <- nhanes_id529 %>%
  rename(race_eth = race_ethnicity,
         sbp = mean_BP, 
         ht = height,
         wt = weight) %>%
  select(id, age, race_eth, sbp:ht) %>% 
  # Take a random sample of 2300 (out of the total 2339 observations)
  slice_sample(., n = 2300) %>% 
  as_tibble()

# Dataset 2: Demographic and clinical parameters (maintain all 2339 observations)
pfas <- nhanes_id529 %>% 
  arrange(id) %>% 
  select(id, matches("pf.*")) %>% 
  as_tibble()

# LOOK AT THE DATA -------------------------------------------------------------
# Print the tibbles to the console
clinical 
pfas

# Alternatively, use glimpse()
glimpse(clinical)
glimpse(pfas)

# PRACTICE ---------------------------------------------------------------------
# 1. Review the code used to create the above datasets and add a comment
# above each line describing what the function is doing


# 2. Left join the clinical data to the pfas data. What happend? 
# How many rows and columns were in the original datasets? 
# How many are in the final, joined dataset? 
left_join(clinical, pfas, by = "id")

# 3. Right join the clinical data to the pfas data. What happend now? 
# Again, how many rows and columns were in the original datasets? 
# And how many are in the final, joined dataset? 
### Add your code here

# 4. Explore one other mutating join (e.g, inner_join, full_join). How 
# do these two joins differ from left and right joins? 
### Add your code here

# 5. Implement the following anti_join function. What information does this provide? 
# How might this be useful? 
anti_join(pfas, clinical, by = "id")

# Load the nhanes data
ID529data::nhanes_id529 %>% 
  # Convert our dataframe to a tibble
  as_tibble() %>% 
  # Select the id and pfas columns
  select(id, PFOS:PFDE) %>% 
  # Make all of the column names lower case
  rename_with(~ str_to_lower(.)) %>%
  # Now we filter for observations that have at least one pfas biomarker measure.
  # rowSums(is.na(.) is summing the number of missing values, across columns, for each row.
  # So, for a given observation, if all pfas values are missing (i.e., rowSums(is.na(.)) == 5), 
  # Then we remove hat observation from our dataset. The result of this filtering will 
  # preserve all observations with *at least* 1 pfas biomarker measure in the dataset.
  filter(rowSums(is.na(.)) < 5) %>%
  mutate(missing_pfas = rowSums(is.na(.))) %>% 
  arrange(desc(missing_pfas))


