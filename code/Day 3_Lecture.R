# 3.1 ---------------------------------------------------------------------
library(tidyverse)    #用tidyverse這個package
# 如果檔名結尾是 .rda
load("C:/Users/Cheng/Desktop/Harvard/2025 Spring/ID 529 Data Management and Analytic Workflows in R/ID529_Lecture/data/Day 3/nhanes_id529.rda")
#賦值並轉換為 tibble 格式 (關鍵步驟！)
our_data <- as_tibble(nhanes_id529)

# Print first 10 rows
print(our_data, n = 10)
# Take our data, which is stored as a tibble
data <- our_data |>
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
view(data)


# 3.2 ---------------------------------------------------------------------

#????


# 3.3 ---------------------------------------------------------------------










