
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


