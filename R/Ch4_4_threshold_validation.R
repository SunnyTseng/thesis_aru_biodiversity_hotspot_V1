# library -----------------------------------------------------------------


library(tidyverse)
library(here)
library(seewave)
library(tuneR)
library(shiny)
library(purrr)
# import data -------------------------------------------------------------
load("bird_data_cleaned_target.RData")



# listen, watch, validate -------------------------------------------------
set.seed(100)

threshold_all <- bird_data_cleaned_target %>%
  mutate(conf_group = cut(confidence, breaks = seq(0.1, 1, by = 0.05), include.lowest = TRUE)) %>%
  slice_sample(n = 20, replace = TRUE, by = c(common_name, scientific_name, conf_group))

write_csv(threshold_all, here("data", "threshold_validation", "threshold_validation_all.csv"))



# test --------------------------------------------------------------------

files <- list.files(here("data", "output_test"), full.names = TRUE) %>%
  map_df(~ read_csv(file = .)) 

write_csv(files, here("data", "output_test", "output_test_all.csv"))

