

# library -----------------------------------------------------------------

library(tidyverse)
library(here)

library(spOccupancy)
library(terra)
library(stars)
library(scales)




# import data -------------------------------------------------------------

# bird detection data
load(here("data", "BirdNET_detections", "bird_data_cleaned_target.rda"))


# effort data
load(here("data", "effort", "effort_site_date.RData"))

effort <- effort_eval_1 %>%
  filter(datetime %within% interval(ymd("2020-06-01"), ymd("2020-06-30")) |
           datetime %within% interval(ymd("2021-06-01"), ymd("2021-06-30")) | 
           datetime %within% interval(ymd("2022-06-01"), ymd("2022-06-30"))) %>%
  mutate(period = floor_date(datetime, unit = "5 days")) %>%
  distinct(site, period) 


# covariate data for modelling
cov_lidar <- readxl::read_xlsx(here("data", "JPRF_lidar_2015", 
                                    "JPRF_veg_Lidar_2015_summarized.xlsx"), 
                               sheet = "100") %>%
  rename(site = Site) %>%
  mutate(site = str_replace(site, "^N(\\d+)", "N_\\1"))
  

# covariate data for prediction
cov_prediction <- rast(here("data", "JPRF_lidar_2015", 
                            "JPRF_veg_Lidar_2015_raw", "Crown_Closure_above_10m_zero1.tif")) %>%
  aggregate(fact = 15, fun = mean) %>%
  as.data.frame(xy = TRUE) %>%
  as_tibble() %>%
  rename(cc10 = Crown_Closure_above_10m_zero1)





# OSFL occurrence data ----------------------------------------------------

OSFL_occ_0 <- bird_data_cleaned_target %>%
  
  # retain qualified detections
  filter(common_name == "Olive-sided Flycatcher") %>%
  filter(confidence >= 0.35) %>%
  
  # retain detection within study period
  filter(datetime %within% interval(ymd("2020-06-01"), ymd("2020-06-30")) |
           datetime %within% interval(ymd("2021-06-01"), ymd("2021-06-30")) | 
           datetime %within% interval(ymd("2022-06-01"), ymd("2022-06-30"))) %>%
  
  # check OSFL detection (1 or 0)
  mutate(period = floor_date(datetime, unit = "5 days")) %>%
  summarize(detections = n(), .by = c(site, period)) %>%
  right_join(effort) %>%
  mutate(detections = if_else(is.na(detections), 0, 1)) %>%
  arrange(period) %>%
  
  # check OSFL detections (NA or keep original value)
  pivot_wider(id_cols = site, 
              names_from = period, 
              values_from = detections) %>%
  arrange(site)


# turn into a matrix and to reflect missing visit
OSFL_occ <- OSFL_occ_0 %>%
  column_to_rownames(var = "site") %>%
  as.matrix() 



# visualization 
OSFL_vis <- OSFL_occ_0 %>%
  pivot_longer(-site, names_to = "period", values_to = "detections") %>%
  mutate(detections = replace_na(detections, -1),
         period = ymd(period),
         year = year(period)) %>%
  # plot
  ggplot(aes(x = period, y = site, fill = factor(detections))) +
  geom_tile() + 
  scale_fill_manual(values = c("white", "lightsteelblue1", "darkslategrey")) +
  facet_wrap(~ year, scales = "free_x") + 
  
  scale_x_date(breaks = scales::pretty_breaks(n = 3), # Automatically choose ~3 breaks
               date_labels = "%b%d") +
  scale_y_discrete(guide = guide_axis(n.dodge = 2)) +
  
  theme_bw() +
  labs(x = "Date", y = "Site") +
  theme(legend.position = "none",
        strip.background = element_rect(fill = "azure3"),
        strip.text.x = element_text(size = 12),
        
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 12),
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 5, r = 0, b = 0, l = 0)))

OSFL_vis

# ggsave(filename = here("docs", "figures", "occupancy_matrix.png"),
#        width = 28,
#        height = 20,
#        units = "cm",
#        dpi = 300)




# occurrence covariates ---------------------------------------------------

OSFL_cov <- cov_lidar %>%
  right_join(OSFL_occ_0) %>%
  select(cc10) 



# detection covariates ----------------------------------------------------

J <- nrow(OSFL_occ_0)
K <- ncol(OSFL_occ_0)

yday <- matrix(NA, nrow = J, ncol = K-1)
for (j in 1:J) {
  for (k in 2:K) {
    if (!is.na(OSFL_occ_0[j, k])) {
      yday[j, k -1] <- names(OSFL_occ_0)[k] %>% as_date() %>% yday()
    }
  }
}


OSFL_det <- list(
  yday = yday
)









  


# spOccupancy modelling ---------------------------------------------------

# data
OSFL_data <- list(y = OSFL_occ,
                  occ.covs = OSFL_cov,
                  det.covs = OSFL_det)

str(OSFL_data)

# formula
OSFL_occ_formula <- ~ scale(cc10)
OSFL_det_formula <- ~ yday


OSFL_inits <- list(alpha = 0, 
                   beta = 0, 
                   z = apply(OSFL_data$y, 1, max, na.rm = TRUE))

OSFL_priors <- list(alpha.normal = list(mean = 0, var = 2.72), 
                    beta.normal = list(mean = 0, var = 2.72))

n.samples <- 5000
n.burn <- 3000
n.thin <- 2
n.chains <- 3

# model
out <- PGOcc(occ.formula = OSFL_occ_formula, 
             det.formula = OSFL_det_formula, 
             data = OSFL_data, 
             inits = OSFL_inits, 
             n.samples = n.samples, 
             priors = OSFL_priors, 
             n.omp.threads = 1, 
             verbose = TRUE, 
             n.report = 1000, 
             n.burn = n.burn, 
             n.thin = n.thin, 
             n.chains = n.chains)




# Goodness of fit ---------------------------------------------------------

summary(out)

plot(out, 'beta', density = FALSE) # Occupancy parameters.

plot(out, 'alpha', density = FALSE) # Detection parameters.




# prediction --------------------------------------------------------------

X.0 <- cbind(1, scale(cov_prediction$cc10))
out.pred <- predict(out, X.0)


plot.dat <- data.frame(x = cov_prediction$x, 
                       y = cov_prediction$y, 
                       mean.psi = apply(out.pred$psi.0.samples, 2, mean), 
                       sd.psi = apply(out.pred$psi.0.samples, 2, sd), 
                       stringsAsFactors = FALSE)
# Make a species distribution map showing the point estimates,
# or predictions (posterior means)

dat.stars <- plot.dat %>%
  filter(mean.psi > 0.2) %>%
  st_as_stars(dims = c('x', 'y'))



ggplot() + 
  geom_stars(data = dat.stars, aes(x = x, y = y, 
                                   fill = mean.psi)) +
  #scale_fill_viridis_c(option = "plasma", na.value = "transparent") +
  scale_fill_gradient(low = "white", high = "lightsteelblue4", na.value = "transparent") +
  labs(x = 'Easting', y = 'Northing', fill = '', 
       title = 'Mean OSFL occurrence probability') +
  theme_bw()

# plot the cov_prediction
ggplot() + 
  geom_raster(data = cov_prediction, aes(x = x, y = y, fill = cc10)) +
  scale_fill_viridis_c(option = "plasma") +
  labs(x = 'Easting', y = 'Northing', fill = '', 
       title = 'Crown Closure above 10m (%)') +
  theme_bw()




