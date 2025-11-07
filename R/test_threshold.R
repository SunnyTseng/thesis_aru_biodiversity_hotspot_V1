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

cov_3 <- rast(here("data", "JPRF_lidar_2015_target_layer", "lidst_le_dis2"))

cov_prediction <- c(cov_1, cov_2, cov_3) %>%
  as.data.frame(xy = TRUE) %>%
  as_tibble() %>%
  rename(cc10 = Crown_Closure_above_10m_zero1, vdi_95 = vdr95,
         d_lid_rip_wet_str_le = lidst_le_dis2)


# Hill data after summarized

load(here("data", "iNEXT_model", "Hills.rda"))


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




# Method 0: get the Hill number of each site ------------------------------


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





# Step 1: Variable selection ----------------------------------------------

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
  
  # filter out the sites with low estimated richness
  filter(estimated > 20) %>%
  
  # combine the LiDAR covariates 
  left_join(cov_lidar, by = join_by(site)) %>%
  
  # check the transformation
  mutate(d_vr_ipolyedge = log(d_vr_ipolyedge + 0.0001),
         d_lid_rip_wet_str_le = log(d_lid_rip_wet_str_le + 0.0001),
         dem = log(dem + 0.0001),
         less10 = log(less10 + 0.0001),
         slope = log(slope + 0.0001)) %>%
  
  # final selection
  select(site, observed, estimated,
         dem, slope, aspect, d_vr_ipolyedge, d_lid_rip_wet_str_le,
         cc1_3, cc3_10, cc10, chm, vdi_95, 
         less10, age80, 
         prop_decid_100m, decid_dens, conf_dens, tree_dens, ba_dens_x100) 




# Check which vaariable needs transformation

# Select predictors + response
plot_data <- diversity_model_data %>%
  select(estimated, dem, slope, aspect, d_vr_ipolyedge, d_lid_rip_wet_str_le,
         cc1_3, cc3_10, cc10, chm, vdi_95, less10, age80,
         prop_decid_100m, decid_dens, conf_dens, tree_dens, ba_dens_x100)

# Make all predictors long for facet plotting
plot_data_long <- plot_data %>%
  pivot_longer(-estimated, names_to = "predictor", values_to = "value")

# Faceted scatterplots with trend lines
ggplot(plot_data_long, aes(x = value, y = estimated)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "loess", se = FALSE, color = "red") +
  facet_wrap(~ predictor, scales = "free_x") +
  theme_minimal(base_size = 12)





# visualization of site richness ------------------------------------------

# First, order sites by observed richness
plot_data <- diversity_model_data %>%
  arrange(desc(observed)) %>%
  mutate(site = factor(site, levels = site))  # preserve order for ggplot

# Plot observed and estimated
ggplot(plot_data, aes(x = site)) +
  geom_point(aes(y = observed, color = "Observed"), size = 2) +
  geom_point(aes(y = estimated, color = "Estimated"), size = 2) +
  scale_color_manual(values = c("Observed" = "#1b9e77", "Estimated" = "#d95f02")) +
  labs(x = "Site (ranked by observed richness)",
       y = "Species richness",
       color = "Type") +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))












# fit the full model and all the possible combinations of the variables
predictor_vars <- diversity_model_data %>%
  select(-1, -2, -3) %>%
  colnames() %>%
  paste(collapse = " + ") 

options(na.action = "na.fail")
full <- lm(as.formula(paste("estimated", "~", predictor_vars)), 
                      data = diversity_model_data)

# all possible combinations of the variables
res <- dredge(full, trace = 2)

# save(object = res,
#      file = here("data", "iNEXT_model", "model_dredge_res.rda"))


# get the importance of each of the variable by their sum of weights
importance_cov <- sw(res)


# get the average of the model
average_cov <- model.avg(res)

summary(average_cov)

average_cov$coefficients[1,] %>% enframe() %>% rename(coefficient = value)

coefficient_plot <- ggcoef_model(average_cov)


# model weight plot
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
  scale_x_discrete(guide = guide_axis(angle = 45)) +
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
       width = 20,
       height = 15,
       units = "cm",
       dpi = 300)





# Step 2: Build linear model for prediction -------------------------------


# build some candidate models based on the importance of the variables
model_1 <- lm(estimated ~ aspect, 
             data = diversity_model_data)

model_2 <- lm(estimated ~ aspect + d_vr_ipolyedge, 
             data = diversity_model_data)

model_3<- lm(estimated ~ aspect + d_vr_ipolyedge + cc1_3, 
            data = diversity_model_data)

# model selection
summary(model_1)
summary(model_2) 
summary(model_3)

AIC(model_1)
AIC(model_2)
AIC(model_3) 



# Step 3: Prediction and visualization ------------------------------------

# Create a data frame for prediction
richness_site_pred <- diversity_model_data %>%
  mutate(pred_lm = predict(model_1),
         pred_loess = predict(loess_mod))


# Plot
ggplot(richness_site_pred, 
       aes(x = d_lid_rip_wet_str_le, y = estimated)) +
  geom_point() +
  geom_smooth(method = "loess",   
              span = 1,          
              formula = y ~ x, 
              color = "blue", se = TRUE) +
  labs(x = "Distance to stream, wetland, and lake edge (m)", 
       y = "Asymptotic richness") +
  theme_bw()



# Step 4: Prediction across the JPRF region -------------------------------


# predict the richness across the whole landscape
richness_map_pred <- predict(loess_mod, cov_prediction, type = "response")

richness_map_pred_1 <- richness_map_pred %>%
  pmax(0) %>%  # set negative values to 0
  round()      # round to the nearest integer

richness_map_vis <- data.frame(x = cov_prediction$x, 
                               y = cov_prediction$y, 
                               richness = richness_map_pred_1)

dat.stars <- richness_map_vis %>%
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



