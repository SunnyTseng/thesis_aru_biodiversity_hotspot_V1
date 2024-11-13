###
### Produce a validated detections dataset for analysis: 
### 1. Only use detections when that ARU is functioning properly
### 2. Only retain the species within the defined species list
### 3. Only keep the detections that meet species-specific thresholds
###
### Author: Sunny Tseng
### Date: 2024 Aug 20
###


# library -----------------------------------------------------------------

library(tidyverse)
library(here)


# import data -------------------------------------------------------------

bird_list <- read_csv(here("data", "bird_list", "species_list_final.csv"))

## load the whole detection dataset "bird_data_cleaned"
load(here("data", "BirdNET_detections", "bird_data_cleaned.RData"))

## load the effort dataset "effort_eval_1"
load(here("data", "effort", "effort_site_date.RData"))



# produce a full dataset for target species -------------------------------

## only keep the validated species, where date/site (effort) that has been 
## properly functioning. Filter species, and the date/site that is working

bird_data_cleaned_target <- bird_data_cleaned %>%
  filter(scientific_name %in% bird_list$scientific_name) %>%
  inner_join(effort_eval_1, by = join_by(site, date == datetime)) %>%
  mutate(datetime = date) %>%
  mutate(id = row_number()) %>%
  select(id, site, datetime, start, end, 
         scientific_name, common_name, confidence, filepath)

save(bird_data_cleaned_target, 
     file = here("data", "BirdNET_detections", "bird_data_cleaned_target.rda"))

## produce a table showing target species information: family, species, 
## species-specific threshold, detections before filtering, detections after filtering, 
## how many sites that have observed the species













