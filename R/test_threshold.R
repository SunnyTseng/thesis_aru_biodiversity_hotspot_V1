### determine the threshold for each species


# library -----------------------------------------------------------------

library(tidyverse)
library(readxl)
library(here)

library(scales)
library(terra)
library(stars)

library(iNEXT)


# functions ---------------------------------------------------------------

# find precision given a threshold
threshold2precision <- function(probability_data, threshold){
  threshold <- probability_data %>%
    filter(confidence > threshold) %>%
    pull(probability) %>%
    mean()
}


# determine the threshold given specified precision level
precision2threshold <- function(threshold_table, precision){
  model <- glm(precision ~ threshold, 
               data = threshold_table,
               family = binomial)
  
  (log(precision/(1 - precision)) - model$coefficients[1])/model$coefficients[2]
}





# import data -------------------------------------------------------------

# validation data
data_validation <- list.files(here("data", "validation_output_files"), 
                              full.names = TRUE) %>%
  map_dfr(~ {
    if (grepl("\\.csv$", .x, ignore.case = TRUE)) {
      read_csv(.x, col_types = cols(.default = "c")) # Read CSV
    } else if (grepl("\\.xlsx$", .x, ignore.case = TRUE)) {
      read_excel(.x, col_types = "text") # Read Excel
    } else {
      stop("Unsupported file format: ", .x)
    }
  })



data_validation_cleaned <- data_validation %>%
  select(-matches("^\\.\\.\\.")) %>%
  mutate(id = as.numeric(id),
         datetime = as_datetime(datetime),
         start = as.numeric(start),
         end = as.numeric(end),
         confidence = as.numeric(confidence)) %>%
  mutate(validation = case_when(validation == "Y" ~ 1,
                                validation == "N" ~ 0,
                                validation == "U" ~ -1))



# full dataset
load(here("data", "BirdNET_detections", "bird_data_cleaned_target.rda"))


# target species list
target_species <- read_excel(here("docs", "random", "species_table.xlsx")) %>%
  filter(used == "Y") %>%
  select(family, scientific_name, common_name, used)


# effort data
load(here("data", "effort", "effort_site_date.RData"))

effort_5 <- effort_eval_1 %>%
  filter(datetime %within% interval(ymd("2020-06-01"), ymd("2020-07-15")) |
           datetime %within% interval(ymd("2021-06-01"), ymd("2021-07-15")) | 
           datetime %within% interval(ymd("2022-06-01"), ymd("2022-07-15"))) %>%
  mutate(period = floor_date(datetime, unit = "5 days")) %>%
  distinct(site, period) 

effort_1 <- effort_eval_1 %>%
  mutate(date = date(datetime)) %>%
  distinct(site, date) %>%
  summarize(site_ARU_days = n(), .by = site)



# covariate data for modelling
cov_lidar <- read_xlsx(here("data", "JPRF_lidar_2015", 
                            "JPRF_veg_Lidar_2015_summarized.xlsx"), 
                       sheet = "100") %>%
  rename(site = Site) %>%
  mutate(site = str_replace(site, "^N(\\d+)", "N_\\1"))



# covariate data for prediction
cov_1 <- rast(here("data", "JPRF_lidar_2015", "JPRF_veg_Lidar_2015_raw", 
                   "Crown_Closure_above_10m_zero1.tif")) 

cov_2 <- rast(here("data", "JPRF_lidar_2015", "JPRF_veg_Lidar_2015_raw", 
                   "vdr95.tif"))

cov_prediction <- c(cov_1, cov_2) %>%
  as.data.frame(xy = TRUE) %>%
  as_tibble() %>%
  rename(cc10 = Crown_Closure_above_10m_zero1, VDI = vdr95)


# get the threshold for each species -------------------------------------


# set up for the for loop
species_list <- data_validation_cleaned %>%
  pull(common_name) %>%
  unique()

thresholds <- numeric(length(species_list))
names(thresholds) <- species_list


# loop across the full species list
for (species in species_list) {
  
  # Check the species dataset are good for modelling
  species_data <- data_validation_cleaned %>%
    filter(common_name == species) %>%
    filter(validation != -1)
  
  if (mean(species_data$validation == 1, na.rm = TRUE) > 0.95) { # more than 95% of the validation is true
    message(paste("All values in validation column are 1:", species))
    thresholds[species] <- 0.1
    next
  }

  
  # Attempt to fit the model and make predictions back to the full dataset
  precision_table <- tryCatch(
    {
      model <- species_data %>%
        glm(validation ~ confidence, 
            data = ., 
            family = binomial) 
      
      probability <- bird_data_cleaned_target %>%
        filter(common_name == species) %>%
        mutate(probability = predict(model, newdata = ., type = "response"))
      
      tibble(threshold = seq(0, 1, 0.01)) %>%
        mutate(precision = map_dbl(.x = threshold, 
                                   .f = ~ threshold2precision(probability, .x))) 
    },
    
    
    warning = function(w) {
      #message(paste("Warning for species:", species, "-", conditionMessage(w)))
      return(NULL)  # Return NULL for warning
    },
    
    
    error = function(e) {
      #message(paste("Error for species:", species, "-", conditionMessage(e)))
      return(NULL)  # Return NULL for errors
    }
  )
  
  
  # Handle the result of tryCatch
  if (is.null(precision_table)) {
    message(paste("Skipping species due to issues in model fitting and prediction:", species))
    thresholds[species] <- 1
    next  # Skip the rest of the loop for this species
  }
  
  
  # Assign the calculated threshold
  thresholds[species] <- precision2threshold(precision_table, 0.95)
  thresholds <- pmax(thresholds, 0.1)
  thresholds <- pmin(thresholds, 1)
}



# filter the dataset based on the threshold -------------------------------

threshold_df <- tibble(
  common_name = names(thresholds),
  threshold = thresholds
)


bird_data_cleaned_target_threshold <- bird_data_cleaned_target %>%
  left_join(threshold_df) %>%
  filter(confidence >= threshold) %>%
  filter(scientific_name %in% target_species$scientific_name) 




# Method 1: get the Hill number of each site ------------------------------


# general data summary 
Hills <- bird_data_cleaned_target_threshold %>%
  
  # data wrangling
  mutate(date = date(datetime)) %>%
  group_nest(site) %>%
  mutate(species_matrix = map(.x = data, .f =~ .x %>%
                                group_by(common_name) %>%
                                summarize(species_ARU_days = n_distinct(date)))) %>%
  select(-data) %>%
  left_join(effort_1) %>%
  
  # create incidence_freq data
  mutate(incidence_freq = map2(.x = site_ARU_days, .y = species_matrix,
                               .f =~ c(.x, .y$species_ARU_days) %>%
                                 setNames(., c("Total", .y$common_name)))) %>%
  
  # remove the sites that don't have enough ARU days
  filter(site_ARU_days >= 30)



# visualization of the ARU days that one specie got observed
Hills_vis <- Hills %>%
  select(-incidence_freq) %>%
  unnest(species_matrix) %>%
  
  # plot
  ggplot(aes(x = site, y = common_name, fill = species_ARU_days)) +
  geom_tile() + 
  
  # fine tune
  scale_x_discrete(guide = guide_axis(n.dodge = 3),
                   position = "top") +
  scale_fill_distiller(palette = "YlOrRd", direction = 1) +
  
  # set theme
  theme_bw() +
  labs(x = "Site", y = "Species") +
  theme(legend.position = "right",
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 12),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 0, r = 0, b = 10, l = 0)))

Hills_vis



# data formation for the iNEXT calculation  
Hills_incidence_freq <- setNames(Hills$incidence_freq, Hills$site) 



# richness plot & table
iNEXT_richness_model <- iNEXT(Hills_incidence_freq, 
                              q = 0,
                              datatype = "incidence_freq")

iNEXT_richness_table <- iNEXT_richness_model$DataInfo %>%
  as_tibble() %>%
  rename(site = Assemblage,
         ARU_days = "T",
         species_ARU_days = "U") %>%
  select(site, ARU_days, species_ARU_days, S.obs, SC) %>%
  left_join(ChaoRichness(Hills_incidence_freq, 
                         datatype = "incidence_freq", 
                         conf = 0.95) %>%
              as_tibble(rownames = "site")) %>%
  rename(observed = Observed,
         estimated = Estimator,
         est_95_lower = `95% Lower`,
         est_95_upper = `95% Upper`) %>%
  select(site, ARU_days, species_ARU_days, 
         observed, estimated, est_95_lower, est_95_upper)



iNEXT_richness_plot <- ggiNEXT(iNEXT_richness_model, 
                               type = 1,
                               se = FALSE) + 
  labs(x = "ARU days", y = "# of species") +
  
  # set theme
  theme_bw() +
  theme(legend.position = "none",
        legend.position.inside = c(0.8, 0.2),
        legend.text = element_text(size = 16),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 12),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)))

iNEXT_richness_plot



# Method 1: build the GLM -------------------------------------------------

diversity_model_data <- iNEXT_richness_table %>%
  left_join(cov_lidar, by = join_by(site)) %>%
  mutate(cc10_scale = scale(cc10),
         VDI_scale = scale(VDI),
         Estimator_int = round(Estimator))

model <- glm(Estimator_int ~ cc10 + VDI,
             data = diversity_model_data, 
             family = "poisson")

summary(model)






# Method 1: GLM model prediction ------------------------------------------

X.0 <- cov_prediction %>%
  mutate(cc10_scale = scale(cc10),
         VDI_scale = scale(VDI)) 

richness_pred <- predict(model, X.0, type = "response")



# Make a species richness map based on predicted values
richness_vis <- data.frame(x = cov_prediction$x, 
                           y = cov_prediction$y, 
                           richness = richness_pred)


dat.stars <- richness_vis %>%
  st_as_stars(dims = c('x', 'y'))


ggplot() + 
  geom_stars(data = dat.stars, aes(x = x, y = y, 
                                   fill = richness)) +
  #scale_fill_viridis_c(option = "plasma", na.value = "transparent") +
  scale_fill_gradient(low = "white", 
                      high = "lightsteelblue4",
                      na.value = "transparent") +
  labs(x = 'Easting', y = 'Northing', 
       fill = 'Richness', title = 'Predicted asymptotic richness') +
  theme_bw()



# plot the cov_prediction
ggplot() + 
  geom_raster(data = cov_prediction, aes(x = x, y = y, fill = cc10)) +
  scale_fill_viridis_c(option = "plasma") +
  labs(x = 'Easting', y = 'Northing', fill = '', 
       title = 'Crown Closure above 10m (%)') +
  theme_bw()






# Methoed 2: get the richness matrix (response variable) -----------------------------


# get the richness list for functioning days
diversity <- bird_data_cleaned_target_threshold %>%
  # retain detection within study period
  # check richness in each site + period combination
  mutate(period = floor_date(datetime, unit = "5 days")) %>%
  summarise(richness = n_distinct(common_name), .by = c(site, period)) %>%
  right_join(effort_5) %>% 
  mutate(richness = if_else(is.na(richness), 0, richness)) %>%
  arrange(period)


# visualization 
diversity_vis <- diversity %>%
  mutate(period = ymd(period),
         year = year(period)) %>%
  
  # plot
  ggplot(aes(x = period, y = site, fill = richness)) +
  geom_tile() + 
  
  
  # fine-tune
  scale_x_date(breaks = scales::pretty_breaks(n = 3), # Automatically choose ~3 breaks
               date_labels = "%b%d") +
  scale_y_discrete(guide = guide_axis(n.dodge = 2)) +
  scale_fill_continuous(type = "viridis") +
  facet_wrap(~ year, scales = "free_x") + 
  
  # set theme
  theme_bw() +
  labs(x = "Date", y = "Site") +
  theme(legend.position = "bottom",
        strip.background = element_rect(fill = "azure3"),
        strip.text.x = element_text(size = 12),
        
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 12),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 5, r = 0, b = 0, l = 0)))

diversity_vis





# Method 2: fit the glm for (richness values & LiDAR covariate) ---------------------

diversity_model_data <- diversity %>%
  left_join(cov_lidar, by = join_by(site)) %>%
  mutate(yday = yday(period),
         year = year(period) %>% as_factor(),
         cc10_scale = scale(cc10),
         VDI_scale = scale(VDI))

model <- glm(richness ~ cc10_scale + VDI_scale + year + yday,
             data = diversity_model_data, 
             family = "poisson")

summary(model)




# Method 2: use the glm to predict the richness map ---------------------------------
X.0 <- cov_prediction %>%
  mutate(cc10_scale = scale(cc10),
         VDI_scale = scale(VDI),
         year = 2021 %>% as_factor(),
         yday = 180) 

richness_pred <- predict(model, X.0, type = "response")



# Make a species richness map based on predicted values
richness_vis <- data.frame(x = cov_prediction$x, 
                           y = cov_prediction$y, 
                           richness = richness_pred)


dat.stars <- richness_vis %>%
  st_as_stars(dims = c('x', 'y'))


ggplot() + 
  geom_stars(data = dat.stars, aes(x = x, y = y, 
                                   fill = richness)) +
  #scale_fill_viridis_c(option = "plasma", na.value = "transparent") +
  scale_fill_gradient(low = "white", 
                      high = "lightsteelblue4",
                      na.value = "transparent") +
  labs(x = 'Easting', y = 'Northing', 
       fill = 'Richness', title = 'Species Richness') +
  theme_bw()



# plot the cov_prediction
ggplot() + 
  geom_raster(data = cov_prediction, aes(x = x, y = y, fill = cc10)) +
  scale_fill_viridis_c(option = "plasma") +
  labs(x = 'Easting', y = 'Northing', fill = '', 
       title = 'Crown Closure above 10m (%)') +
  theme_bw()










