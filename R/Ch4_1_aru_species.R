###
### Purpose: create JPRF ARU bird list (threshold = 0.85)
### 
### Author: Sunny Tseng
### Date: 2023-07-04
### Input: 
### Output: 
###

###
### Library
###
library(seewave)
library(tuneR)
library(tidyverse)
library(here)

###
### Detection list from 2020, 2021, and 2022 passerine
###
file_names <- c("2020_passerine_BirdNET_updated.csv", 
                "2021_passerine_BirdNET.csv",
                "2022_passerine_BirdNET.csv")

data_all <- tibble()
for (file in file_names){
  data <- read_csv(here("data", "processed", file))
  data_all <- bind_rows(data_all, data)
}

# clean out the detection with confidence higher than 0.85, got 126 unique species
data_all_0.85 <- data_all %>%
  filter(confidence >= 0.85) %>%
  mutate(common_name = case_when(
    common_name == "Audubon's Warbler" ~ "Yellow-rumped Warbler",
    common_name == "Northwestern Crow" ~ "American Crow",
    common_name == "Slate-colored Fox Sparrow" ~ "Fox Sparrow",
    common_name == "Sooty Fox Sparrow" ~ "Fox Sparrow",
    common_name == "American Yellow Warbler" ~ "Yellow Warbler",
    TRUE ~ common_name)) 

# make a list of potential recordings based on individual species (126 species for validation)
species_recording <- data_all_0.85 %>%
  unite("recording_u", site, recording, start_s, end_s, sep = "_") %>%
  arrange(desc(confidence)) %>%
  group_nest(common_name) %>%
  mutate(recording_1 = map_chr(.x = data, .f =~ .x %>% pull(recording_u) %>% .[1]),
         recording_2 = map_chr(.x = data, .f =~ .x %>% pull(recording_u) %>% .[2]),
         recording_3 = map_chr(.x = data, .f =~ .x %>% pull(recording_u) %>% .[3]),
         recording_4 = map_chr(.x = data, .f =~ .x %>% pull(recording_u) %>% .[4]),
         recording_5 = map_chr(.x = data, .f =~ .x %>% pull(recording_u) %>% .[5])) %>%
  select(-data) # it's 129 now because I removed the Fox Sparrow. 

# save the list for further listening validation
# write_csv(species_recording, here("data", "species_aru_85_1.csv"))

###
### listen to the target recordings
###
# species <- 126 # range from 1 to 126 for 126 species
# 
# for (info in 2:6) { # don't change this, it is for the column number
#   file <- species_recording[species, info] %>%
#     str_split(pattern = "_") %>%
#     unlist()
#   
#   dir <- paste0("E:/Audio/", str_sub(file[3], 1,4), "_passerine")
#   site <- paste0(file[1], "_", file[2])
#   recording <- paste0(file[3], "_", file[4])
#   start_s <- file[5] %>% as.numeric()
#   end_s <- file[6] %>% as.numeric()
#   song <- readWave(paste0(dir, "/", site, "/", recording, ".wav"), 
#                    from = (start_s - 2), 
#                    to = (end_s + 2), 
#                    units = "seconds")
#   
#   print(paste0("This is recording ", recording, " from ", start_s, " to ", end_s))
#   play(song, ... = "/play /close")
# }
# 
# # plot out spectrum whenever needed
# spec <- spectro(song, flim = c(0, 5), tlim = c(2, 3))  


###
### Filter out false detections base on listening result, expert opinion, and fix the name 
### 

# Filter out the non-validated species and species (108) suggested by Ken that shouldn't be here (103)
data_validated <- read_csv(here("data", "JPRF_species_list",  "species_aru_85_validation_1.csv")) %>%
  drop_na(best) %>%
  filter(!common_name %in% c("Pygmy Nuthatch", 
                             "Red-naped Sapsucker", 
                             "Williamson's Sapsucker", 
                             "Bay-breasted Warbler", 
                             "Bushtit", 
                             "American Tree Sparrow",
                             "Arctic Warbler",
                             "Blue-headed Vireo",
                             "Connecticut Warbler")) 

clements_species <- read_csv(here("data", "JPRF_species_list", "Clements-Checklist-v2022-October-2022.csv"))

data_species <- data_all_0.85 %>%
  mutate(confidence_cat = cut(confidence, breaks = seq(from = 0.1, to = 1.0, by = 0.15))) %>%
  group_nest(common_name) %>%
  mutate(c_10_25 = map_dbl(.x = data, .f =~ .x %>% filter(confidence_cat == "(0.1,0.25]") %>% nrow()),
         c_25_40 = map_dbl(.x = data, .f =~ .x %>% filter(confidence_cat == "(0.25,0.4]") %>% nrow()),
         c_40_55 = map_dbl(.x = data, .f =~ .x %>% filter(confidence_cat == "(0.4,0.55]") %>% nrow()),
         c_55_70 = map_dbl(.x = data, .f =~ .x %>% filter(confidence_cat == "(0.55,0.7]") %>% nrow()),
         c_70_85 = map_dbl(.x = data, .f =~ .x %>% filter(confidence_cat == "(0.7,0.85]") %>% nrow()),
         c_85_100 = map_dbl(.x = data, .f =~ .x %>% filter(confidence_cat == "(0.85,1]") %>% nrow()),
         n_ARU = map_dbl(.x = data, .f =~ .x %>% pull(site) %>% n_distinct())) %>%
  mutate(c_10_25_2020 = map_dbl(.x = data, .f =~ .x %>% filter(year == 2020) %>% filter(confidence_cat == "(0.1,0.25]") %>% nrow()),
         c_25_40_2020 = map_dbl(.x = data, .f =~ .x %>% filter(year == 2020) %>% filter(confidence_cat == "(0.25,0.4]") %>% nrow()),
         c_40_55_2020 = map_dbl(.x = data, .f =~ .x %>% filter(year == 2020) %>% filter(confidence_cat == "(0.4,0.55]") %>% nrow()),
         c_55_70_2020 = map_dbl(.x = data, .f =~ .x %>% filter(year == 2020) %>% filter(confidence_cat == "(0.55,0.7]") %>% nrow()),
         c_70_85_2020 = map_dbl(.x = data, .f =~ .x %>% filter(year == 2020) %>% filter(confidence_cat == "(0.7,0.85]") %>% nrow()),
         c_85_100_2020 = map_dbl(.x = data, .f =~ .x %>% filter(year == 2020) %>% filter(confidence_cat == "(0.85,1]") %>% nrow()),
         n_ARU_2020 = map_dbl(.x = data, .f =~ .x %>% filter(year == 2021) %>% pull(site) %>% n_distinct())) %>%
  mutate(c_10_25_2021 = map_dbl(.x = data, .f =~ .x %>% filter(year == 2021) %>% filter(confidence_cat == "(0.1,0.25]") %>% nrow()),
         c_25_40_2021 = map_dbl(.x = data, .f =~ .x %>% filter(year == 2021) %>% filter(confidence_cat == "(0.25,0.4]") %>% nrow()),
         c_40_55_2021 = map_dbl(.x = data, .f =~ .x %>% filter(year == 2021) %>% filter(confidence_cat == "(0.4,0.55]") %>% nrow()),
         c_55_70_2021 = map_dbl(.x = data, .f =~ .x %>% filter(year == 2021) %>% filter(confidence_cat == "(0.55,0.7]") %>% nrow()),
         c_70_85_2021 = map_dbl(.x = data, .f =~ .x %>% filter(year == 2021) %>% filter(confidence_cat == "(0.7,0.85]") %>% nrow()),
         c_85_100_2021 = map_dbl(.x = data, .f =~ .x %>% filter(year == 2021) %>% filter(confidence_cat == "(0.85,1]") %>% nrow()),
         n_ARU_2021 = map_dbl(.x = data, .f =~ .x %>% filter(year == 2021) %>% pull(site) %>% n_distinct())) %>%
  mutate(c_10_25_2022 = map_dbl(.x = data, .f =~ .x %>% filter(year == 2022) %>% filter(confidence_cat == "(0.1,0.25]") %>% nrow()),
         c_25_40_2022 = map_dbl(.x = data, .f =~ .x %>% filter(year == 2022) %>% filter(confidence_cat == "(0.25,0.4]") %>% nrow()),
         c_40_55_2022 = map_dbl(.x = data, .f =~ .x %>% filter(year == 2022) %>% filter(confidence_cat == "(0.4,0.55]") %>% nrow()),
         c_55_70_2022 = map_dbl(.x = data, .f =~ .x %>% filter(year == 2022) %>% filter(confidence_cat == "(0.55,0.7]") %>% nrow()),
         c_70_85_2022 = map_dbl(.x = data, .f =~ .x %>% filter(year == 2022) %>% filter(confidence_cat == "(0.7,0.85]") %>% nrow()),
         c_85_100_2022 = map_dbl(.x = data, .f =~ .x %>% filter(year == 2022) %>% filter(confidence_cat == "(0.85,1]") %>% nrow()),
         n_ARU_2022 = map_dbl(.x = data, .f =~ .x %>% filter(year == 2022) %>% pull(site) %>% n_distinct())) %>%
  select(-data)


data_validated_1 <- left_join(data_validated, data_species) %>%
  left_join(clements_species, by = c("common_name" = "English name")) %>%
  select(common_name, "scientific name", "order", "family", 14:41)

# save the extended validation file
# write_csv(data_validated_1, here("data", "JPRF_species_list", "species_aru_85_validation_info_1.csv"))


###
### Save the detection based files
### 

# only keep the validated species
# only keep the date/site (effort) that has been properly functional
data_all_0.85_species <- data_all_0.85 %>%
  filter(common_name %in% data_validated_1$common_name) %>%
  left_join(clements_species, by = c("common_name" = "English name"))  %>%
  mutate(month = as.numeric(month),
         hour = str_sub(recording, start = 10, end = 11) %>% as.numeric(),
         minute = str_sub(recording, start = 12, end = 13) %>% as.numeric()) %>%
  filter(year >= 2020 & year <= 2022,
         month >= 5 & month <= 7,
         hour %in% c(4, 5, 6),
         minute %in% seq(0, 60, by = 5)) %>%
  select(year, month, day, site, recording, start_s, end_s, common_name, order, family, "scientific name")

write_csv(data_all_0.85_species, here("data", "detection_aru_target_sp_85.csv"))


###
### Creating folders and move soundfiles around for the validated sounds (108 species)
###

# ready for for loop. Here we are doing a species for first iteration
for (j in 1:nrow(data_validated_1)){
  
  species <- data_validated_1[j,]
  
  # create file folder
  dir.create(here("data", "JPRF_summer_bird_list_and_sound", species$common_name))
  
  # For single species, iteration for 5 recordings (if true) and rename the best one
  for (i in 7:11) {
    if (species[i] == "Y") {
      
      file <- species[i-5] %>%
        str_split(pattern = "_") %>%
        unlist()
      
      year <- str_sub(file[3], 1,4)
      site <- paste0(file[1], "_", file[2])
      recording <- paste0(file[3], "_", file[4])
      dir <- paste0("E:/Audio/", year, "_passerine")
      
      from_file <- paste0(dir, "/", site, "/", recording, ".WAV")
      to_folder <- here("data", "JPRF_summer_bird_list_and_sound", species$common_name)
      
      # file copy and rename
      file.copy(from_file, to_folder)
      
      file.rename(paste0(here("data", "JPRF_summer_bird_list_and_sound", species$common_name), "/", recording, ".WAV"),
                  paste0(here("data", "JPRF_summer_bird_list_and_sound", species$common_name), "/", year, "_", site, "_", recording, ".WAV"))
    }
  }
  
  best_index <- as.numeric(species[12]) + 6
  
  file <- species[best_index - 5] %>%
    str_split(pattern = "_") %>%
    unlist()
  
  year <- str_sub(file[3], 1,4)
  site <- paste0(file[1], "_", file[2])
  recording <- paste0(file[3], "_", file[4])
  dir <- paste0("E:/Audio/", year, "_passerine")
  
  file.rename(paste0(here("data", "JPRF_summer_bird_list_and_sound", species$common_name), "/", year, "_", site, "_", recording, ".WAV"),
              paste0(here("data", "JPRF_summer_bird_list_and_sound", species$common_name), "/", year, "_", site, "_", recording, "_A", ".WAV"))
}

