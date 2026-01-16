# this secret message is medium difficulty 

# you should download the data.csv file and render it as a scatter plot in R, but you'll need to take the square root of X1 and X2 to view it right

# we'll even give you the ggplot2 code to run, but it's missing a few steps

library(tidyverse)
library(magrittr)

df <- readr::read_csv("data/Day 5/data_level 2.csv", col_names = F)

ggplot(df, aes(sqrt(X1), X2, color = X2)) + 
  geom_point() + 
  theme_void()+
  scale_color_viridis_c(option='inferno', end= .8)