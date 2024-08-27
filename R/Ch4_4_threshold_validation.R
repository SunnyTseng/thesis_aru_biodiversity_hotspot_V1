# library -----------------------------------------------------------------
library(tidyverse)
library(here)
library(seewave)
library(tuneR)


# import data -------------------------------------------------------------
load("bird_data_cleaned_target.RData")



# listen, watch, validate -------------------------------------------------
set.seed(100)

data_validation <- bird_data_cleaned_target %>%
  mutate(conf_group = cut(confidence, breaks = seq(0.1, 1, by = 0.05), include.lowest = TRUE)) %>%
  group_by(common_name, conf_group) %>%
  summarize(n())
  
  
  slice_sample(n = 50, replace = TRUE, by = c(common_name, scientific_name, conf_group))



















library(gt)

car_data <- mtcars[1:5, 1:4]  # Select first 5 rows and 4 columns

gt_table <- gt(data = car_data) %>%
  tab_header(
    title = "Car Dataset",
    subtitle = "A Subset of the mtcars Dataset"
  ) %>%
  fmt_number(
    columns = vars(mpg, disp),
    decimals = 1
  ) %>%
  cols_label(
    mpg = "Miles/Gallon",
    cyl = "Cylinders",
    disp = "Displacement",
    hp = "Horsepower"
  )

# Display the table with customizations
gt_table
