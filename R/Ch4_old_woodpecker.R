

library(tidyverse)
library(here)

load(here("data", "BirdNET_detections", "bird_data_cleaned_target.rda"))

bird_list <- read_csv(here("data", "bird_list", "Clements-v2024-October-2024-rev.csv"))


test <- bird_data_cleaned_target %>%
  left_join(bird_list, by = c("common_name" = "English name")) %>%
  filter(family == "Picidae (Woodpeckers)") %>%
  mutate(date = date(datetime)) %>%
  select(id, site, date, datetime, start, end,
         family, scientific_name, common_name, confidence)

write_csv(test, here("data", "BirdNET_detections", "bird_data_cleaned_target_woodpeckers.csv"))

test_summary <- test %>%
  group_by(scientific_name, common_name) %>%
  summarise(n_detections = n(),
            n_ARU_days = n_distinct(date),
            n_sites = n_distinct(site))
