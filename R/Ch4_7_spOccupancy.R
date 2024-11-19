

# library -----------------------------------------------------------------

library(tidyverse)
library(here)



# import data -------------------------------------------------------------

# bird detection data
load(here("data", "BirdNET_detections", "bird_data_cleaned_target.rda"))


# effort data
load(here("data", "effort", "effort_site_date.RData"))

effort <- effort_eval_1 %>%
  mutate(year = year(datetime),
         week = week(datetime)) %>%
  distinct(site, year, week) 


# covariate data


# test for OSFL -----------------------------------------------------------

OSFL_occ <- bird_data_cleaned_target %>%
  
  # retain qualified detections
  filter(common_name == "Olive-sided Flycatcher") %>%
  filter(confidence >= 0.35) %>%
  
  # check OSFL detections for each week (1 or 0)
  mutate(year = year(datetime),
         week = week(datetime)) %>%
  summarize(detections = n(), .by = c(year, week, site)) %>%
  right_join(effort) %>%
  mutate(detections = if_else(is.na(detections), 0, 1)) %>%
  
  # turn into a matrix and to reflect missing visit
  unite(col = "year_week", year, week) %>%
  pivot_wider(id_cols = site, 
              names_from = year_week, 
              values_from = detections)



# occurrence covariates ---------------------------------------------------









  



