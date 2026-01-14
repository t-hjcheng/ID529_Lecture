# 3.1 ---------------------------------------------------------------------
library(tidyverse)    #用tidyverse這個package

# Print first 10 rows
print(our_data, n = 10)
# Take our data, which is stored as a tibble
new_data <- our_data |>
  # THEN filter our data for a subset of rows
  filter(group == "i <3 dplyr") |>
  # THEN arrange the result by number of gold stars
  arrange(desc(num_gold_stars))

glimpse(data, width = 50)
# Explicitly rename variables in the dataset
rename(data, 
       sbp = mean_BP,
       pov_ratio = poverty_ratio,
       race_eth = race_ethnicity,
) |> 
  glimpse()








