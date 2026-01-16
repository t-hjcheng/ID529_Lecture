# for this challenge, download the data, read it in, and plot it in ggplot2 but use a 
# facet wrap to get each of the distinct messages

# you might start with something like this before adding the facet_wrap:

library(tidyverse)

df <- readr::read_csv("data/Day 5/data_level 3.csv")

ggplot(df, aes(x = X1, y = X2, color=msg)) + 
  geom_point(size=.75) + 
  facet_wrap(~msg, labeller=function(x) return(''))
  theme_void() +
  theme(legend.position = 'none')
  panel.background = element_rect(fill='white')
  plot.background= element_rect(fill='white')
  
ggsave("output/Day 5/data_level 3.png")