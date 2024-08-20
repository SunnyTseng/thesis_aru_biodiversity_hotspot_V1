###
### Produce a validated detections dataset for analysis: 
### 1. Only use detections when that ARU is functioning properly
### 2. Only retain the species within the defined species list
### 3. Only keep the detections that meet species-specific thresholds
###
### Author: Sunny Tseng
### Date: 2024 Aug 20
###



# produce a full dataset for target species -------------------------------

## only keep the validated species, where date/site (effort) that has been 
## properly functioning. Filter species, year, month, hour of a day. 
species_list_final <- read_csv(here("data", "Bird_list", "species_list_final.csv"))

bird_data_filtered_species_list <- bird_data_cleaned %>%
  filter(scientific_name %in% species_list_final$scientific_name) %>%
  filter(date %within% interval(ymd("2020-05-01"), ymd("2020-07-31")) | 
           date %within% interval(ymd("2021-05-01"), ymd("2021-07-31")) | 
           date %within% interval(ymd("2022-05-01"), ymd("2022-07-31"))) %>%
  filter(date %>% hour() >= 4 & date %>% hour() <= 7)