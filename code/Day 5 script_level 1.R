# this one is the easiest of the group 

# you should download the data.csv file and render it as a scatter plot in R

# we'll even give you the ggplot2 code to run: 

library(tidyverse)
library(magrittr)

df <- readr::read_csv("data/Day 5/data_level 1.csv", col_names = F)

df %>% ggplot(aes(x = X1, y = X2, color= X1)) + 
  geom_point() + 
  theme_void()
  scale_color_distiller(palette = "Spectral")+
  theme(legend.position = "none", background = element_rect(fill ="black"))