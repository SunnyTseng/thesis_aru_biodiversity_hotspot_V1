
# library -----------------------------------------------------------------

library(tidyverse)
library(here)
library(gt)



# load data ---------------------------------------------------------------

# bird list in JPRF
species_list_final <- read_csv(here("data", "bird_list", "species_list_final.csv"))

# bird list from eBird to get the order and family
clement <- read_csv(here("data", "bird_list", "Clements-v2024-October-2024-rev.csv"))

# bird detections to get the detection number
load(here("data", "BirdNET_detections", "bird_data_cleaned_target.rda"))

# bird detections for final list
load(here("data", "BirdNET_detections", "bird_data_cleaned_target_threshold.rda"))

# iNEXT richness
load(here("data", "iNEXT_model", "iNEXT_richness_model.rda"))



# bird parameters table ---------------------------------------------------

birdnet_argument <- c("i", "o", "lat", "lon", "week", "slist", "sensitivity",
                      "min_conf", "overlap", "rtype", "threads", "batchsize",
                      "locale", "sf_thresh", "classifier", "fmin", "fmax",
                      "output_file", "skip_existing_results")

default_value <- c("None", "None", "-1", "-1", "-1", "None", "1.0", "0.1",
                   "0", "table", "1", "1", "en", "0.03", "None", "0",
                   "15000", "None", "FALSE")

my_value <- c("--", "--", " ", " ", " ", " ", " ",
              " ", " ", "r", "4", "4",
              " ", " ", " ", " ", " ", "--", "TRUE")


birdnet_table <- data.frame(Argument = birdnet_argument,
                            Default = default_value,
                            MyValue = my_value,
                            stringsAsFactors = FALSE) %>% 
  gt() %>%
  cols_label(Argument = "Argument",
             Default = "Default value",
             MyValue = "Used value") %>%
  cols_align(align = "center") %>%
  tab_options(table.font.size = 12,
              heading.title.font.size = 16,
              heading.subtitle.font.size = 12)

gtsave(data = birdnet_table, filename = here("docs", "tables", "birdnet_parameters.rtf"))



# bird species table ------------------------------------------------------

species_table <- bird_data_cleaned_target %>%
  
  # get basic info for detections
  summarise(no_detections = n(),
            no_sites = n_distinct(site),
            .by = c(common_name, scientific_name)) %>%
  
  # get order and family info
  mutate(scientific_name = if_else(scientific_name == "Accipiter gentilis", 
                                   "Astur atricapillus", 
                                   scientific_name)) %>%
  left_join(clement, by = join_by(scientific_name == `scientific name`)) %>%
  select(order, family, scientific_name, `English name`, no_detections, no_sites) %>%
  rename(common_name = `English name`) %>%
  arrange(order, family) %>%
  
  # make table
  gt() %>%
  cols_label(order = "Order",
             family = "Family",
             scientific_name = "Scientific name",
             common_name = "Common name",
             no_detections = "No. detections",
             no_sites = "No. sites") %>%
  cols_align(align = "center") %>%
  tab_options(table.font.size = 12) %>%
  tab_style(style = cell_borders(sides = "bottom", color = "white", weight = px(1)),
    locations = cells_body()) %>%
  tab_style(style = cell_borders(sides = "bottom", color = "black", weight = px(1)),
            locations = cells_body(rows = family != lead(family, default = last(family))))
  
gtsave(data = species_table, 
       filename = here("docs", "tables", "species_table.rtf"))











# bird species table after applying threshold -----------------------------

species_table_threshold <- bird_data_cleaned_target_threshold %>%
  
  # filter the data for the site that ended up not using
  filter(!site %in% c("14_27", "14_32", "N_02")) %>%
  
  # get basic info for detections
  summarise(no_detections = n(),
            no_sites = n_distinct(site),
            threshold = unique(threshold),
            .by = c(common_name, scientific_name)) %>%
  
  # get names cleaned, for changing names
  mutate(common_name = if_else(common_name == "Pacific-slope Flycatcher", 
                                   "Western Flycatcher", 
                                   common_name)) %>%
  mutate(threshold = round(threshold, 2)) %>%
  relocate(threshold, .after = scientific_name) %>%
  arrange(desc(no_sites), desc(no_detections)) %>%
  
  # make table
  gt() %>%
  cols_label(scientific_name = "Scientific name",
             common_name = "Common name",
             threshold = "Threshold (95%)",
             no_detections = "No. detections",
             no_sites = "No. sites") %>%
  cols_align(align = "center") %>%
  tab_options(table.font.size = 12) 

gtsave(data = species_table_threshold, 
       filename = here("docs", "tables", "species_table_threshold.rtf"))




# asymptotic richness for each site --------------------------------

iNEXT_richness_table <- iNEXT_richness_model$DataInfo %>%
  
  # table arrangement
  as_tibble() %>%
  left_join(iNEXT_richness_model$AsyEst %>% 
              filter(Diversity == "Species richness") %>%
              rename(S.obs = Observed)) %>%
  mutate(Estimator = round(Estimator, 2),
         s.e. = round(s.e., 2)) %>%
  select(Assemblage, "T", "U", 
         "S.obs", "Estimator", "s.e.") %>%
  arrange(T, U) %>%
  
  # make table
  gt() %>%
  cols_label(Assemblage = "Site",
             "T" = "ARU days",
             "U" = "Species ARU days",
             "S.obs" = "No. species observed",
             "Estimator" = "Asymptotic richness",
             "s.e." = "Bootstrap SE") %>%
  cols_align(align = "center") %>%
  tab_options(table.font.size = 12) 
  

gtsave(data = iNEXT_richness_table,
       filename = here("docs", "tables", "iNEXT_richness_table.rtf"))



