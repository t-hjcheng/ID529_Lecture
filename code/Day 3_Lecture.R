# 3.1 ---------------------------------------------------------------------
library(tidyverse)    #用tidyverse 這個package
# Print first 10 rows
print(our_data, n = 10)
# Take our data, which is stored as a tibble
new_data <- our_data |>
  # THEN filter our data for a subset of rows
  filter(group == "i <3 dplyr") |>
  # THEN arrange the result by number of gold stars
  arrange(desc(num_gold_stars))


