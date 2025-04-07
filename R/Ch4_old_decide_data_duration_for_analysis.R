

# library -----------------------------------------------------------------

library(tidyverse)
library(here)



# import data -------------------------------------------------------------

load(here("data", "effort", "effort_site_date.RData"))

load(here("data", "BirdNET_detections", "bird_data_cleaned_target.rda"))



# find stable period of detections ----------------------------------------

ARU_effort <- effort_eval_1 %>%
  mutate(date = date(datetime)) %>%
  group_by(date) %>%
  distinct(site, date) %>%
  summarize(n_ARU = n())

normalized_count <- bird_data_cleaned_target %>%
  mutate(date = date(datetime)) %>%
  summarize(n_detections = n(),
            n_species = n_distinct(common_name), .by = date) %>%
  left_join(ARU_effort) %>%
  mutate(n_detections_ARU = n_detections/n_ARU,
         n_species_ARU = n_species/n_ARU)

vis <- normalized_count %>%
  mutate(year = year(date)) %>%
  ggplot() +
  geom_point(aes(x = date, y = n_detections_ARU, group = year)) + 
  facet_wrap(~ year, scale = "free")

vis

