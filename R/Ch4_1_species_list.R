###
### Create JPRF ARU bird list (threshold = 0.85)
### Author: Sunny Tseng
### Date: 2024-07-19
###


# library -----------------------------------------------------------------

library(seewave)
library(tuneR)
library(tidyverse)
library(here)



# load data ---------------------------------------------------------------

## species list in BC atlas
species_list_BC <- read_csv(here("data", "Bird_list", "species_bc_breeding_bird_atlas.csv")) %>%
  mutate(common_name = if_else(common_name == "Gray Jay", "Canada Jay", common_name))

# bird detections from 3 years of data
# bird_data <- list.files(here("data", "Audio_output_combined"),
#                         pattern = ".csv$", recursive = TRUE,
#                         full.names = TRUE) %>%
#   map_df(~ read_csv(file = .)) 
# 
# bird_data_cleaned <- bird_data %>%
#   mutate(site = str_split_i(filepath, pattern = "\\\\", i = -2),
#          recording = str_split_i(filepath, pattern = "\\\\", i = -1)) %>%
#   mutate(date = str_split_i(recording, pattern = ".WAV", i = 1) %>% as_datetime()) 
#
# save(bird_data_cleaned, file = here("bird_data_cleaned.RData"))

load(here("bird_data_cleaned.RData"))


# species list validation with 0.975 --------------------------------------

## save the list for further listening validation
bird_data_0.975 <- bird_data_cleaned %>%
   filter(confidence >= 0.975) %>% # 155 species
   slice_max(order_by = confidence, n = 5, by = scientific_name, with_ties = FALSE)
# write_csv(bird_data_0.975, here("data", "Bird_list", "species_validation_raw.csv"))

## listen to the target recordings
bird_data_0.975 <- read_csv(here("data", "Bird_list", "species_validation_raw.csv"))
for (i in 521:550) {

  print(paste0("This is ", bird_data_0.975$common_name[i],
               " for row ", i + 1,
               " from ", bird_data_0.975$start[i],
               " to ", bird_data_0.975$end[i]))

  song <- readWave(paste0(bird_data_0.975$filepath[i]),
                   from = (bird_data_0.975$start[i] - 2),
                   to = (bird_data_0.975$end[i] + 2),
                   units = "seconds") %>%
    play(, ... = "/play /close")
}



# producing the species list from the validated data ----------------------

## basic species list: the intersection of the BirdNET_0.85 and the BC list
species_list_basic <- bird_data_cleaned %>%
  filter(confidence >= 0.8) %>%
  distinct(scientific_name, common_name) %>%
  inner_join(species_list_BC)

## additional species list: all the species detected above 0.975 and validated. 
## Adding this list to the above. There are 136 species in total.
species_list_additional <- read_csv(here("data", "Bird_list", 
                                         "species_validation_processed.csv")) %>%
  filter(any(validation == "Y"), .by = scientific_name) %>%
  distinct(scientific_name, common_name) %>%
  full_join(species_list_basic) 
  
## remove the species that won't happen in the area, ask Ken
## All the species that Ken suggested are not in the BC list.
## Other than Red-naped Sapsucker, which I am retaining the species. A total
## of 13 species were droped
species_list_final <- species_list_additional %>%
  drop_na(code)
# write_csv(species_list_additional, here("data", "Bird_list",
#                                         "species_list_final.csv"))


