# Round two of trying to find the pattern
# There is no trend in terms of the "breeding song bird richness", but how about dividing into further smaller group of species, such as flycatchers?


# library -----------------------------------------------------------------

library(tidyverse)
library(readxl)
library(janitor)
library(here)

library(scales)
library(terra)
library(stars)
library(ggcorrplot2)
library(patchwork)
library(car) # for VIF calculation

library(psych) # for correlation calculation
library(iNEXT)
library(MuMIn)


# load the data -----------------------------------------------------------

load(here("data", "BirdNET_detections",
          "bird_data_cleaned_target_threshold.rda"))

load(here("data", "clustering", 
          "kmeans_species_cluster_3.rda"))

species_clusters <- data.frame(common_name = names(km$cluster),
                               cluster = km$cluster)

bird_data_clustered <- bird_data_cleaned_target_threshold %>%
  left_join(species_clusters, by = "common_name")




# effort data
load(here("data", "effort", "effort_site_date.RData"))

effort_1 <- effort_eval_1 %>%
  mutate(date = date(datetime)) %>%
  distinct(site, date) %>%
  summarize(site_ARU_days = n(), .by = site)



# covariate data for modelling
cov_lidar <- read_xlsx(here("data", "JPRF_lidar_2015", 
                            "JPRF_veg_Lidar_2015_summarized.xlsx"), 
                       sheet = "100") %>%
  clean_names() %>%
  mutate(site = str_replace(site, "^N(\\d+)", "N_\\1")) %>%
  rename(
    d_vri_polyedge = d_vr_ipolyedge,
    less_10 = less10,
    age_80 = age80,
    prop_decid = prop_decid_100m,
    ba_dens = ba_dens_x100
  )



# calculating the Hill numbers --------------------------------------------

# general data summary 
Hills <- bird_data_clustered %>%
  #filter(cluster == 1) %>%
  
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
  filter(site_ARU_days >= 35) 



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
  
  # combine the LiDAR covariates 
  left_join(cov_lidar, by = join_by(site)) %>%
  
  # check the transformation
  mutate(d_vri_polyedge = log(d_vri_polyedge + 0.1),
         d_lid_rip_wet_str_le = log(d_lid_rip_wet_str_le + 0.1),
         dem = log(dem + 0.1),
         less_10 = log(less_10 + 0.1),
         slope = log(slope + 0.1)) %>%
  mutate(aspect_sin = sin(aspect * pi / 180),
         aspect_cos = cos(aspect * pi / 180)) %>%
  
  # final selection
  select(site, observed, estimated, aspect_sin, aspect_cos,
         dem, slope, aspect, d_lid_rip_wet_str_le, d_vri_polyedge, 
         cc1_3, cc3_10, cc10, chm, vdi_95,
         less_10, age_80, prop_decid, decid_dens, conf_dens, tree_dens, ba_dens) 

#

# exploratory data analysis -----------------------------------------------

### check correlation - full
ct <- diversity_model_data %>%
  select(dem, slope, aspect, d_lid_rip_wet_str_le, d_vri_polyedge, 
         cc1_3, cc3_10, cc10, chm, vdi_95,
         less_10, age_80, prop_decid, decid_dens, conf_dens, tree_dens, ba_dens) %>%
  corr.test(adjust = "none")

c.mat <- ct$r
p.mat <- ct$p

colnames(c.mat) <- c("dem", "slope", "aspect", "d_lid", "d_vri", 
                     "cc1_3", "cc3_10", "cc10", "chm", "vdi95",
                     "lt10", "age80", "prop_decid", "dec_dens", 
                     "conf_dens", "tree_dens", "ba_dens")

# visualization
var_selec_1 <- ggcorrplot.mixed(c.mat, 
                 upper = "ellipse", lower = "number", 
                 p.mat = p.mat, 
                 insig = "blank") +
  theme(legend.position = "none")

# save the figure
ggsave(plot = var_selec_1,
       filename = here("docs", "figures", "fig_var_selec_1.png"),
       width = 16,
       height = 16,
       units = "cm",
       dpi = 300)


### check correlation - after selection
ct <- diversity_model_data %>%
  select(dem, slope, aspect, d_lid_rip_wet_str_le, d_vri_polyedge, 
         cc1_3, cc3_10, cc10,
         age_80, prop_decid) %>%
  corr.test(adjust = "none")

c.mat <- ct$r
p.mat <- ct$p

colnames(c.mat) <- c("dem", "slope", "aspect", "d_lid", "d_vri", 
                     "cc1_3", "cc3_10", "cc10", 
                     "age80", "prop_decid")

# visualization
var_selec_2 <- ggcorrplot.mixed(c.mat, 
                 upper = "ellipse", lower = "number", 
                 p.mat = p.mat, 
                 insig = "blank") +
  theme(legend.position = "none")

# save the figure
ggsave(plot = var_selec_2,
       filename = here("docs", "figures", "fig_var_selec_2.png"),
       width = 14,
       height = 14,
       units = "cm",
       dpi = 300)


### check VIFs for correlation
lm_vif_filtered <- lm(estimated ~ dem + slope + aspect + 
                        d_lid_rip_wet_str_le + d_vri_polyedge + 
                        cc1_3 + cc3_10 + cc10 +
                        age_80 + prop_decid,
                      data = diversity_model_data)

vif_values <- vif(lm_vif_filtered) %>%
  enframe(name = "Variable", value = "VIF")

# plot the VIF values
var_selec_3 <- ggplot(vif_values) +
  geom_col(aes(x = reorder(Variable, VIF), 
               y = VIF),
           fill = "#7A8B8B") +
  geom_hline(yintercept = 5, linetype = "dashed", 
             color = "red", size = 1.5) +
  
  # fine tune
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  labs(x = NULL, y = "VIF") +
  
  # theme
  theme_minimal(base_size = 13) + 
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 12),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)))


# save the figure
ggsave(plot = var_selec_3,
       filename = here("docs", "figures", "fig_var_selec_3.png"),
       width = 15,
       height = 9,
       units = "cm",
       dpi = 300)



# variable selection ------------------------------------------------------

diversity_model_data <- diversity_model_data %>%
  select(site, observed, estimated,
         dem, slope, aspect_sin, aspect_cos, d_lid_rip_wet_str_le, d_vri_polyedge, 
         cc1_3, cc3_10, cc10,
         age_80, prop_decid) 


plot_data <- diversity_model_data %>%
  select(estimated, dem, slope, aspect_sin, aspect_cos, d_lid_rip_wet_str_le, d_vri_polyedge, 
         cc1_3, cc3_10, cc10,
         age_80, prop_decid) %>%
  pivot_longer(cols = c(dem, slope, aspect_sin, aspect_cos, d_lid_rip_wet_str_le, d_vri_polyedge, 
                        cc1_3, cc3_10, cc10,
                        age_80, prop_decid),
               names_to = "covariate",
               values_to = "value")

ggplot(plot_data, aes(x = value, y = estimated)) +
  geom_point(alpha = 0.5, size = 1) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 0.8) +
  facet_wrap(~ covariate, scales = "free_x") +
  labs(
    x = "LiDAR covariate value",
    y = "Estimated species richness",
    title = "Relationships between estimated richness and LiDAR covariates"
  ) +
  theme_bw() +
  theme(
    strip.text = element_text(size = 10),
    plot.title = element_text(face = "bold")
  )









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

# get the importance of each of the variable by their sum of weights
importance_cov <- sw(res)


# model weight plot
importance_cov_all <- importance_cov %>% 
  
  # wrangle the data
  enframe() %>% 
  mutate(important = if_else(value > 0.8, "Y", "N")) %>%
  mutate(name = as_factor(name)) %>%
  
  # set factor levels explicitly
  mutate(name = factor(name, levels = c("dem", "slope", "aspect_sin", "aspect_cos", 
                                        "d_lid_rip_wet_str_le", "d_vri_polyedge", 
                                        "cc1_3", "cc3_10", "cc10",
                                        "age_80", "prop_decid"))) %>%
  
  # plot
  ggplot(aes(x = name, y = value, fill = important)) +
  geom_bar(stat = "identity") + 
  geom_hline(yintercept = 0.8, linetype = "dashed", color = "red") +
  
  # fine tune
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  scale_fill_manual(values = c("Y" = "#8B5742", "N" = "grey")) +
  theme_bw() +
  labs(x = "LiDAR covariates", y = "Sum of model weights") +
  theme(legend.position = "none",
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 12),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = , l = 0)))



importance_cov_all
importance_cov_purple
importance_cov_orange
importance_cov_green


# save the figure
ggsave(plot = importance_cov_purple,
       filename = here("docs", "figures", "importance_cov_purple.png"),
       width = 12,
       height = 8,
       units = "cm",
       dpi = 300)


# relationship between variable and estimated richness --------------------

# prediction for purple group
model_purple <- lm(estimated ~ cc10, 
                   data = diversity_model_data)

prediction_cov_purple <- diversity_model_data %>%
  # main plot
  ggplot(aes(x = cc10, y = estimated)) +
  geom_smooth(method = "lm", linewidth = 2,
              colour = "#7A67EE", fill = "#CAE1FF") +
  geom_point(colour = "#473C8B") +
  ylim(12, 14) +
  # fine tune
  theme_bw() +
  labs(x = "Average crown closure above 10m", 
       y = "Asymptotic richness") +
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 12),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = , l = 0)))
  

# prediction for orange group 
model_orange <- lm(estimated ~ prop_decid, 
                   data = diversity_model_data)

prediction_cov_oragne <- diversity_model_data %>%
  # main plot
  ggplot(aes(x = prop_decid, y = estimated)) +
  geom_smooth(method = "lm", linewidth = 2,
              colour = "#EE9A00", fill = "peachpuff") +
  geom_point(colour = "#8B5742") +
  ylim(5, 7) +
  # fine tune
  theme_bw() +
  labs(x = "Proportion of deciduous tree counts", 
       y = "Asymptotic richness") +
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 12),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = , l = 0)))


# prediction for green group
model_green <- lm(estimated ~ aspect_sin, 
                   data = diversity_model_data)

prediction_cov_green <- diversity_model_data %>%
  # main plot
  ggplot(aes(x = aspect_sin, y = estimated)) +
  geom_smooth(method = "lm", linewidth = 2,
              colour = "#548B54", fill = "#8FBC8F") +
  geom_point(colour = "#556B2F") +
  #ylim(5, 7) +
  # fine tune
  theme_bw() +
  labs(x = "Aspect (sin)", 
       y = "Asymptotic richness") +
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 12),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = , l = 0)))


# save the figure
prediction_cov_purple
prediction_cov_orange
prediction_cov_green

# save the figure
ggsave(plot = prediction_cov_green,
       filename = here("docs", "figures", "prediction_cov_green.png"),
       width = 10,
       height = 10,
       units = "cm",
       dpi = 300)
