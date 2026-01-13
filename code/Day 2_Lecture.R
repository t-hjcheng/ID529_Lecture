
# 2.1 ---------------------------------------------------------------------
# install devtools if you haven't already
install.packages("devtools") 

# install our ID529tutorials package
devtools::install_github("ID529/ID529tutorials")

library(ID529tutorials)
available_tutorials('ID529tutorials')
run_tutorial('logic', 'ID529tutorials')


# 2.3 ---------------------------------------------------------------------
# SET UP ------------------------------------------------------------------
install.packages(tidyverse) # Includes 'readr' package
install.packages(readxl)    # For importing excel files (xlsx, xls)
install.packages(haven)     # For importing statistical software files (.dta, .sas7bdat, etc.)    

# Load packages 
library(tidyverse)
library(readxl)
library(haven)

# CASE STUDY 1 ------------------------------------------------------------
# Read in the fixed with data file without providing any additional details
data <- read_fwf("data/Day 2/newstudy.txt")

# Print the data in your console
data 

# Another way to print/view data in your console
glimpse(data) 

# Step 1: Create a vector of column widths (from left to right; ncol = 9)
col_widths <- c(5, 2, 2, 1, 2, 5, 5, 3, 3)

# Step 2: Create a vector of column names (as in step 1, from left to right; ncol = 9)
col_names <- c("id", "age", "edu", "smoke", "cigs", "sbp", "dbp", "chol", "glucose")

# Step 3: Read in the data, specifying column names and widths
data <- read_fwf("data/Day 2/newstudy.txt", fwf_widths(widths = col_widths, col_names = col_names))

# Print the data
data 

# CASE STUDY 2 ------------------------------------------------------------
# Read in the xlsx file
data <- read_xlsx("data/Day 2/exercise.xlsx")

# Look at the data
data
glimpse(data)

# Read in the data
# skip = 3 skips the first 3 rows (start reading on 4th row, which includes column names)
# [-1,] drops the first row of data which, here, are text describing units for the column
data <- read_xlsx("data/Day 2/exercise.xlsx", skip = 3)[-1,]

# Look at the data
data
glimpse(data)

# Coerce all but the time column to a numeric data type
data[2:11] <- sapply(data[2:11], as.numeric)

# Standardize the column names (all lower case)
colnames(data) <- tolower(colnames(data))

# Print the data
data

# 2.4 ---------------------------------------------------------------------
flipper_length_over_time <-
data.frame(
year = c(2007, 2007, 2007, 
               2008, 2008, 2008, 
               2009, 2009, 2009),
flipper_length_mm =
      c(186.5, 192.4, 215.1,
         191.0, 197.7, 217.5,
        192.0, 198.0, 218.4), 
    species = as.factor(
      c(
        "Adelie", "Chinstrap", "Gento",
        "Adelie", "Chinstrap", "Gento",
        "Adelie", "Chinstrap", "Gento"
 )))

library(ggplot2)

ggplot(flipper_length_over_time, 
       aes(x = year, 
           y = flipper_length_mm, 
           color = species)) +   # 這裡就是分顏色和畫出三條線的關鍵！
  geom_line()                    # 加上線條圖層


# 2.5 ---------------------------------------------------------------------
library(datapasta)
library(tidyverse)
#Example 3 
harvard_school_ages <- tibble::tribble(
  ~school, ~founding_year,
  "Harvard College",     1636,
  "Medicine",     1782,
  "Divinity",     1816,
  "Law",     1817,
  "Engineering and Applied Sciences",     1847,
  "Dental Medicine",     1867,
  "Arts and Sciences",     1872,
  "Business",     1908,
  "Extension",     1910,
  "Design",     1936,
  "Education",     1920,
  "Public Health",     1913,
  "Government",     1936
)

harvard_school_ages$age <- 2025 - harvard_school_ages$founding_year

ggplot(harvard_school_ages, 
       aes(y = forcats::fct_reorder(school, age), x = age)) + 
  geom_col(fill = 'firebrick') 

ggplot(harvard_school_ages, 
       aes(y = forcats::fct_reorder(school, age), x = founding_year, xend = 2025)) + 
  geom_segment(color = 'firebrick') + 
  geom_point() + 
  geom_vline(xintercept = 2025, linetype = 'dashed', color = 'firebrick') + 
  geom_text(aes(x = (2025 + founding_year)/2, label = paste0(age, ' years old')), nudge_y = .3, size = 3, color = 'firebrick') + 
  geom_text(aes(x = founding_year, label = founding_year), nudge_y = -.3, size = 3) + 
  theme_bw() +
  scale_x_continuous(breaks = c(1700, 1800, 1900, 2000, 2025)) + 
  ggtitle("Ages of Schools at Harvard University in 2025") + 
  labs(y = "", x = "", caption = "Data from https://en.wikipedia.org/wiki/Harvard_University")
