###
### Purpose: create JPRF ARU bird list (threshold = 0.85)
### 
### Author: Sunny Tseng
### Date: 2024-07-19
### Input: 
### Output: 
###


# library -----------------------------------------------------------------

library(seewave)
library(tuneR)
library(tidyverse)
library(here)



# load data ---------------------------------------------------------------

# standard species list 
species_list_full <- read_csv(here("data", "Bird_list", "Clements-v2023-October-2023.csv"))

# bird detections from 3 years of data
bird_data <- list.files(here("data", "Audio_output_combined"),
                        pattern = ".csv$", recursive = TRUE,
                        full.names = TRUE) %>%
  map_df(~ read_csv(file = .)) %>%
  select(filepath, site, date, recording, start, end, scientific_name, common_name, confidence)



# species list validation with 0.9 ----------------------------------------

# save the list for further listening validation
bird_data_0.9 <- bird_data %>%
  filter(confidence >= 0.9) %>%
  slice_max(order_by = confidence, n = 2, by = scientific_name, with_ties = FALSE)

write_csv(bird_data_0.9, here("data", "Bird_list", "species_validation_raw.csv"))


# listen to the target recordings
for (i in 1:nrow(bird_data_0.9)) {
  
  print(paste0("This is ", bird_data_0.9$common_name[i], 
               " for row ", i, 
               " from ", bird_data_0.9$start[i], 
               " to ", bird_data_0.9$end[i]))
  
  song <- readWave(paste0(bird_data_0.9$filepath[i]),
                   from = (bird_data_0.9$start[i] - 2),
                   to = (bird_data_0.9$end[i] + 2),
                   units = "seconds") %>%
    play(, ... = "/play /close")
}


# filter out false detections base on listening result, expert opinion, and fix the name 
species_list <- read_csv(here("data", "Bird_list", "species_validation_processed.csv")) %>%
  group_by(scientific_name) %>%
  filter(any(validation == "Y")) %>%
  distinct(scientific_name, common_name) %>%
  left_join(select(species_list, `scientific name`, `English name`, order, family), 
            by = join_by(scientific_name == `scientific name`))

write_csv(species_list, here("data", "Bird_list", "species_list_aru.csv"))

# c("Pygmy Nuthatch", 
#   "Red-naped Sapsucker", 
#   "Williamson's Sapsucker", 
#   "Bay-breasted Warbler", 
#   "Bushtit", 
#   "American Tree Sparrow",
#   "Arctic Warbler",
#   "Blue-headed Vireo",
#   "Connecticut Warbler"))
  
# common_name == "Audubon's Warbler" ~ "Yellow-rumped Warbler",
# common_name == "Northwestern Crow" ~ "American Crow",
# common_name == "Slate-colored Fox Sparrow" ~ "Fox Sparrow",
# common_name == "Sooty Fox Sparrow" ~ "Fox Sparrow",
# common_name == "American Yellow Warbler" ~ "Yellow Warbler",


           

# produce a full dataset for target species -------------------------------

# only keep the validated species, where date/site (effort) that has been properly functional
bird_data_target <- bird_data %>%
  filter(scientific_name %in% species_list$scientific_name) 

# for effort checking, is there a better way?
# filter(year >= 2020 & year <= 2022,
#        month >= 5 & month <= 7,
#        hour %in% c(4, 5, 6),
#        minute %in% seq(0, 60, by = 5))


write_csv(bird_data_target, here("data", "Bird_list", "bird_data_full_target_species.csv"))







