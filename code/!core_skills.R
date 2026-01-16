# know how to install packages:
# install.packages("tidyverse")

# set up a project so we could use the {here} package

# dependencies ------------------------------------------------------------

library(tidyverse)
library(broom)
library(here)
library(palmerpenguins)
library(gtsummary)


# read in data ------------------------------------------------------------

# we could use a csv dataset like this:
# df <- readr::read_csv(here("data.csv"))

# or use an example dataset like penguins from palmerpenguins:
# use View to look at it in RStudio
View(penguins)


# data manipulation -------------------------------------------------------

# use group_by and summarize together to create summary statistics per-group
penguins_summarized <- penguins |>
  group_by(species) |>
  summarize(
    mean_flipper_length_mm = mean(flipper_length_mm, na.rm=TRUE))

# know how to use mutate to update columns (either creating new ones or updating
# existing ones):
# here, we'll just convert species to a character vector just for an example
# so then we can next practice making it a factor:
penguins <- penguins |>
  mutate(species = as.character(species))

# convert a variable to a factor:
#
# method 1: base R
# here, the levels will be assumed from the output of unique(penguins$species):
penguins$species <- factor(penguins$species)
#
# method 2: dplyr
penguins <- penguins |>
  mutate(species = factor(species))

# if I wanted to change the reference category, I could use relevel:
penguins$species <- relevel(penguins$species, 'Chinstrap')

# or the dplyr way:
penguins <- penguins |>
  mutate(species = relevel(species, 'Chinstrap'))
# you can also use forcats::fct_relevel


# data visualization ------------------------------------------------------

# use ggplot2 to make some graphics
# a scatter plot:
ggplot(penguins, aes(x = bill_length_mm, y = bill_depth_mm, color = species)) +
  geom_point() +
  ggtitle("Penguin Bill Lengths and Depths by Species")

# use ggsave to save your work
ggsave(here("output/penguins_scatterplot.png"), width = 7, height = 5)

# a histogram with facets:
ggplot(penguins, aes(x = flipper_length_mm)) +
  geom_histogram() +
  facet_wrap(~species) +
  ggtitle("Penguin Bill Lengths and Depths by Species")

# again use ggsave and here() to save it within your project
ggsave(here("output/penguins_faceted_histogram.png"), width = 8, height = 3)


# you might also want to plot regression lines in ggplot quickly so
# use geom_smooth:
ggplot(penguins, aes(x = body_mass_g, y = flipper_length_mm)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  ggtitle("Linear regression of flipper length on body mass")


# analyze a model -------------------------------------------------------------

model <- lm(flipper_length_mm ~ body_mass_g + species, penguins)

# use broom::tidy to extract the coefficients and their statistics
model_output <- broom::tidy(model, conf.int = TRUE)

# visualize model results
model_output |>
  filter(term != '(Intercept)') |>
  ggplot(aes(x = estimate, y = term, xmin = conf.low, xmax = conf.high)) +
  geom_pointrange()

# create a table of the results
gtsummary::tbl_regression(model)


# one example with multiple models --------------------------------------------

model1 <- lm(flipper_length_mm ~ species, penguins)
model2 <- lm(flipper_length_mm ~ species + body_mass_g, penguins)
model3 <- lm(flipper_length_mm ~ species + body_mass_g + island, penguins)

# extract tables of results
model_results <- list(
  bind_cols(model = 'model1', broom::tidy(model1, conf.int = TRUE)),
  bind_cols(model = 'model2', broom::tidy(model2, conf.int = TRUE)),
  bind_cols(model = 'model3', broom::tidy(model3, conf.int = TRUE)))

# make into one data frame
model_results <- bind_rows(model_results)

# create a plot of covariates from multiple models
model_results |>
  filter(term %in% c('speciesGentoo', 'speciesAdelie')) |>
  ggplot(
       aes(x = estimate,
           y = term,
           xmin = conf.low,
           xmax = conf.high,
           color = model,
           shape = model)) +
  geom_pointrange(position = position_dodge(width = 0.5)) +
  ggtitle("Coefficient estimates for species effect",
          stringr::str_wrap(
            paste(
              "Model 1 includes no other covariates, model 2 includes body mass,",
              "and model 3 includes body mass and island effects"
            )
          ))
