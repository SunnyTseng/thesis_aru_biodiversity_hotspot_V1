

# library -----------------------------------------------------------------

library(tidyverse)
library(here)



# import data -------------------------------------------------------------

load(here("data", "BirdNET_detections", "bird_data_cleaned_target.rda"))




# test for OSFL -----------------------------------------------------------

OSFL_occ <- bird_data_cleaned_target %>%
  filter(common_name == "Olive-sided Flycatcher") %>%
  filter(confidence >= 0.35) %>%
  mutate(week = week(datetime)) %>%
  select(site, week, )
  



