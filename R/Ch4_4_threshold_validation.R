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

test_file <- threshold_all %>%
  select(filepath, start, end, scientific_name, common_name, confidence) %>%
  filter(common_name == "Barred Owl") %>%
  mutate(validation = "ENTER_VALUE_HERE")

write_csv(test_file, here("data", "output_test", "Barred_Owl_official.csv"))

#write_csv(threshold_all, here("data", "threshold_validation", "threshold_validation_all.csv"))



# test --------------------------------------------------------------------

files <- list.files(here("data", "output_test"), full.names = TRUE) %>%
  map_df(~ read_csv(file = .)) 

short_file <- files %>%
  filter(common_name == "Barred Owl") %>%
  select(filepath, start, end, scientific_name, common_name, confidence) %>%
  mutate(validation = "ENTER_VALUE_HERE")

write_csv(short_file, here("data", "output_test", "Barred_Owl.csv"))

