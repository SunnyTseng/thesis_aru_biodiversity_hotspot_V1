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
library(corrplot)

library(iNEXT)
library(MuMIn)


# load the data -----------------------------------------------------------

load(here("data", "BirdNET_detections",
          "bird_data_cleaned_target_threshold.rda"))

load(here("data", "clustering", 
          "kmeans_species_cluster_3.rda"))

species_clusters <- data.frame(common_name = rownames(data_cluster),
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
  mutate(site = str_replace(site, "^N(\\d+)", "N_\\1"))



# calculating the Hill numbers --------------------------------------------

# general data summary 
Hills <- bird_data_clustered %>%
  #filter(cluster == 2) %>%
  
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
  mutate(d_vr_ipolyedge = log(d_vr_ipolyedge + 0.1),
         d_lid_rip_wet_str_le = log(d_lid_rip_wet_str_le + 0.1),
         dem = log(dem + 0.1),
         less10 = log(less10 + 0.1),
         slope = log(slope + 0.1)) %>%
  mutate(aspect_sin = sin(aspect * pi / 180),
         aspect_cos = cos(aspect * pi / 180)) %>%
  
  # final selection
  select(site, observed, estimated,
         dem, slope, aspect_sin, aspect_cos, d_vr_ipolyedge, d_lid_rip_wet_str_le, 
         cc10, #chm, tree_dens, ba_dens_x100,
         prop_decid_100m, #decid_dens,
         cc3_10, #vdi_95, #cc1_3, less10, 
         age80 #, conf_dens
         ) 

# , chm, cc10, ba_dens_x100, tree_dens, cc1_3, vdi_95, conf_dens



# exploratory data analysis -----------------------------------------------

# check correlation
diversity_model_data %>%
  select(-1, -2, -3) %>%
  cor(use = "complete.obs") %>%
  corrplot(., method = "color", type = "upper",
         tl.col = "black", tl.srt = 45, addCoef.col = "black",
         number.cex = 0.6)

# check VIFs for correlation
lm_vif_filtered <- lm(estimated ~ dem + slope + aspect_sin + aspect_cos + d_vr_ipolyedge + 
                        d_lid_rip_wet_str_le +
                        cc10 + cc3_10 + prop_decid_100m + age80,
                      data = diversity_model_data)
vif(lm_vif_filtered)


# check for potential transformation
plot_data <- diversity_model_data %>%
  select(-1, -2)

# Make all predictors long for facet plotting
plot_data_long <- plot_data %>%
  pivot_longer(-estimated, names_to = "predictor", values_to = "value")

# Faceted scatterplots with trend lines
ggplot(plot_data_long, aes(x = value, y = estimated)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "loess", se = FALSE, color = "red") +
  facet_wrap(~ predictor, scales = "free_x") +
  theme_minimal(base_size = 12)




# variable selection ------------------------------------------------------


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

# save the figure
ggsave(plot = importance_cov_fig,
       filename = here("docs", "figures", "fig_cov_all_species.png"),
       width = 20,
       height = 15,
       units = "cm",
       dpi = 300)
