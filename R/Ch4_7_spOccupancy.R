

# library -----------------------------------------------------------------

library(tidyverse)
library(here)
library(spOccupancy)



# import data -------------------------------------------------------------

# bird detection data
load(here("data", "BirdNET_detections", "bird_data_cleaned_target.rda"))


# effort data
load(here("data", "effort", "effort_site_date.RData"))

effort <- effort_eval_1 %>%
  mutate(year = year(datetime),
         week = week(datetime)) %>%
  distinct(site, year, week) 


# covariate data
load()


# test for OSFL -----------------------------------------------------------

OSFL_occ <- bird_data_cleaned_target %>%
  
  # retain qualified detections
  filter(common_name == "Olive-sided Flycatcher") %>%
  filter(confidence >= 0.35) %>%
  
  # check OSFL detections for each week (1 or 0)
  mutate(year = year(datetime),
         week = week(datetime)) %>%
  summarize(detections = n(), .by = c(year, week, site)) %>%
  right_join(effort) %>%
  mutate(detections = if_else(is.na(detections), 0, 1)) %>%
  
  # turn into a matrix and to reflect missing visit
  unite(col = "year_week", year, week) %>%
  pivot_wider(id_cols = site, 
              names_from = year_week, 
              values_from = detections)



# occurrence covariates ---------------------------------------------------


oven.occ.formula <- ~ scale(Elevation) + I(scale(Elevation)^2)
oven.det.formula <- ~ scale(day) + scale(tod) + I(scale(day)^2)
# Check out the format of ovenHBEF
str(ovenHBEF)



# Format with explicit specification of inits for alpha and beta
# with four detection parameters and three occurrence parameters 
# (including the intercept).
oven.inits <- list(alpha = c(0, 0, 0, 0), 
                   beta = c(0, 0, 0), 
                   z = apply(ovenHBEF$y, 1, max, na.rm = TRUE))

oven.priors <- list(alpha.normal = list(mean = 0, var = 2.72), 
                    beta.normal = list(mean = 0, var = 2.72))

n.samples <- 5000
n.burn <- 3000
n.thin <- 2
n.chains <- 3

out <- PGOcc(occ.formula = oven.occ.formula, 
             det.formula = oven.det.formula, 
             data = ovenHBEF, 
             inits = oven.inits, 
             n.samples = n.samples, 
             priors = oven.priors, 
             n.omp.threads = 1, 
             verbose = TRUE, 
             n.report = 1000, 
             n.burn = n.burn, 
             n.thin = n.thin, 
             n.chains = n.chains)







  



