### determine the threshold for each species


# library -----------------------------------------------------------------

library(tidyverse)
library(readxl)
library(janitor)
library(here)

library(scales)
library(terra)
library(stars)

library(iNEXT)
library(MuMIn)


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
  clean_names() %>%
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
    message(paste("More than 95% of the detections are true regardless of confidence:", species))
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

threshold_df <- tibble(common_name = names(thresholds),
                       threshold = thresholds) %>%
  left_join(target_species) %>%
  drop_na(used)


# filter the detections that don't achieve species-specific threshold
bird_data_cleaned_target_threshold <- bird_data_cleaned_target %>%
  left_join(threshold_df) %>%
  filter(confidence >= threshold) 


# save(bird_data_cleaned_target_threshold, 
#      file = here("data", "BirdNET_detections",
#                  "bird_data_cleaned_target_threshold.rda"))

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
  filter(site_ARU_days >= 15) 

# save(object = Hills,
#      file = here("data", "iNEXT_model", "Hills.rda"))




# iNEXT model and calculation  
iNEXT_richness_model <- iNEXT(setNames(Hills$incidence_freq, Hills$site), 
                              q = 0,
                              datatype = "incidence_freq")

# save(object = iNEXT_richness_model, 
#      file = here("data", "iNEXT_model", "iNEXT_richness_model.rda"))




# Method 1: build the GLM -------------------------------------------------

# prepare the data for modelling
diversity_model_data <- ChaoRichness(setNames(Hills$incidence_freq,
                                              Hills$site), 
                                     datatype = "incidence_freq") %>% 
  
  # clean the table
  as_tibble(rownames = "site") %>%
  rename(observed = Observed,
         estimated = Estimator,
         LCL = `95% Lower`,
         UCL = `95% Upper`) %>%
  
  # bootstrap random sampling for 50 between LCL and UCL -- > 
  # concern: the probability distribution of the real estimate is not uniform 
  # between LCL and UCL, thus, a raodom uniform sampling is not a good idea
  
  # crossing(bootstrap_id = 1:5) %>%
  # rowwise() %>%
  # mutate(estimated_bootstrap = runif(1, LCL, UCL)) %>%
  # ungroup()
  
  # combine the LiDAR covariates 
  left_join(cov_lidar, by = join_by(site)) %>%
  select(estimated,
         dem, slope, aspect, dist_wet_lidar, d_vr_ipolyedge, 
         cc1_3, cc3_10, cc10, chm, vdi_95, 
         less10, age80, 
         prop_decid_100m, decid_dens, conf_dens, tree_dens, ba_dens_x100) 


# fit the full model and all the possible combinations of the variables
predictor_vars <- diversity_model_data %>%
  select(-1) %>%
  colnames() %>%
  paste(collapse = " + ") 

options(na.action = "na.fail")
full <- lm(as.formula(paste("estimated", "~", predictor_vars)), 
                      data = diversity_model_data)

# all possible combinations of the variables
res <- dredge(full, trace = 2) # started 11:10 

# save(object = res,
#      file = here("data", "iNEXT_model", "model_dredge_res.rda"))


# get the importance of each of the variable by their sum of weights
importance_cov <- sw(res)

importance_cov_fig <- importance_cov %>% 
  
  # wrangle the data
  enframe() %>% 
  mutate(name = case_when(
    name == "d_vr_ipolyedge" ~ "d_vri_polyedge",
    name == "less10" ~ "less_10",
    name == "age80" ~ "age_80",
    name == "prop_decid_100m" ~ "prop_decid",
    name == "ba_dens_x100" ~ "ba_dens",
    .default = name)) %>%
  mutate(important = if_else(value > 0.8, "Y", "N")) %>%
  arrange(desc(value)) %>%
  mutate(name = as_factor(name)) %>%
  
  # plot
  ggplot(aes(x = name, y = value, fill = important)) +
  geom_bar(stat = "identity") + 
  geom_hline(yintercept = 0.8, linetype = "dashed", color = "red") +
  
  # fine tune
  scale_x_discrete(guide = guide_axis(n.dodge = 3)) +
  scale_fill_manual(values = c("Y" = "darkblue", "N" = "lightblue")) +
  theme_bw() +
  labs(x = "LiDAR covariates", y = "Sum of model weights") +
  theme(legend.position = "none",
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 12),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = , l = 0)))

importance_cov_fig

ggsave(plot = importance_cov_fig,
       filename = here("docs", "figures", "importance_cov_fig.png"),
       width = 18,
       height = 12,
       units = "cm",
       dpi = 300)




# Method 1: GLM model prediction ------------------------------------------

# get the average of the model
average_cov <- model.avg(res, subset = delta < 4)

coefficient_plot <- ggcoef_model(average_cov)








average_cov$coefficients[1,] %>% enframe() %>% rename(coefficient = value)

intercept = 38.89 
slope = -0.03896

predicted <- intercept + 
  slope * diversity_model_data$d_vr_ipolyedge

estimated <- diversity_model_data$estimated


test_1 <- tibble(predicted = predicted,
                 estimated = estimated) %>%
  ggplot(aes(y = estimated, x = predicted)) +
  geom_point() + 
  geom_abline(intercept = 0, slope = 1)

test_1






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


