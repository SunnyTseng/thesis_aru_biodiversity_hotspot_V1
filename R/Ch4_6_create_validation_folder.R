
# library -----------------------------------------------------------------

library(tidyverse)
library(here)

library(tuneR)
library(seewave)

# import data -------------------------------------------------------------

load(here("data", "BirdNET_detections", "bird_data_cleaned_target.rda"))

# function for moving recordings to a given folder
move_recording <- function(id, filepath, start, end, 
                           buffer = 3, path = species_folder, drive = "H:", ...){
  
  # load audio file
  audio <- readWave(str_replace(filepath, "E:", drive))
  # Trim the audio using start and end times in seconds
  trimmed_audio <- cutw(audio, f = audio@samp.rate, 
                        from = max(0, start - buffer), 
                        to =  min(length(audio@left) / audio@samp.rate, end + buffer), 
                        output = "Wave")
  # Save the trimmed audio if needed
  writeWave(trimmed_audio, file.path(path, 
                                     paste0(target_species, "_", id, ".wav")))
  
}


# isolate recording segments for validation -------------------------------

set.seed(2024)

species <- bird_data_cleaned_target %>%
  pull(common_name) %>% 
  unique() 


# loop through species 
for (target_species in species) {
  # manage species folder
  species_folder <- here("data", "validation_recordings_practice", target_species)
  
  if (!dir.exists(species_folder)) {
    dir.create(species_folder, recursive = TRUE)
  }
  
  # validation table
  table <- bird_data_cleaned_target %>%
    filter(common_name == target_species) %>%
    mutate(category = cut(confidence, breaks = seq(0.1, 1, by = 0.05), right = FALSE)) %>%
    slice_sample(n = 20, by = category) 
  
  write_csv(table, file.path(species_folder, paste0(target_species, "_validation.csv")))
  
  # move recordings
  pmap(table, move_recording)          
}


# run the shinyApp for validation -----------------------------------------

library(shiny) 
library(bslib)
library(shinyWidgets) 
library(shinyFiles)

library(tidyverse)
library(DT)
library(praise)

library(tuneR)
library(seewave)

shiny::runGitHub("Birds-Canada-ARU-2024", "SunnyTseng", subdir = "R")





