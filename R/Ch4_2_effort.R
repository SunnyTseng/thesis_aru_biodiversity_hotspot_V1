###
### Name: Checking the JPRF ARU effort
### Author: Sunny Tseng
### Date: 2024 Aug 20
###


# library -----------------------------------------------------------------

library(tidyverse)
library(here)
library(lubridate)

library(terra) ## to read in data
library(sf) ## to read in csv and transfer to terra object
library(ggmap) ## for getting basemap
library(tidyterra) # to add terra object to ggplot
library(ggspatial) ## to add features on the map
library(bcmaps) ## to get the outline of the BC maps
library(cowplot) ## to combine plots


# functions ---------------------------------------------------------------

get_effort <- function(dir){
  effort_info <- tibble()
  files <- list.files(path = dir, 
                      pattern = 'WAV', 
                      recursive = TRUE, 
                      full.names = TRUE)
  
  effort_info <- tibble(file = files) %>%
    mutate(site = file %>% str_split_i(pattern = "/", -2),
           datetime = file %>% str_split_i(pattern = "/", -1) %>% ymd_hms(),
           size = file %>% file.info() %>% pull(size))
  
  return(effort_info)
}



# check effort: date, time, and file size ---------------------------------

effort_eval <- c("E:/Audio/2020_passerine", 
                 "E:/Audio/2021_passerine",
                 "E:/Audio/2022_passerine") %>%
  map_df(~ get_effort(dir = .))
save(effort_eval, file = here("effort_eval.RData"))

effort_eval_1 <- effort_eval %>%
  ## this line can be removed after running the updated function
  mutate(datetime = file %>% str_split_i(pattern = "/", -1) %>% ymd_hms()) %>%
  filter(datetime %within% interval(ymd("2020-05-01"), ymd("2020-07-31")) | 
         datetime %within% interval(ymd("2021-05-01"), ymd("2021-07-31")) | 
         datetime %within% interval(ymd("2022-05-01"), ymd("2022-07-31"))) %>%
  filter(datetime %>% hour() >= 4 & datetime %>% hour() <= 7) %>%
  filter(size >= 5760000 & size <= 5760500)


# no. of active ARUs for each of the date ---------------------------------

## data wrangling and getting ready for the plot
vis_wrangling <- effort_eval_1 %>%
  group_by(date = datetime %>% date()) %>%
  summarize(ARUs = n_distinct(site)) %>%
  mutate(year = year(date),
         md = paste("1994", month(date), day(date), sep = "-") %>% ymd()) %>%
  select(-date) 

all <- vis_wrangling %>% expand(year, md)

vis_wrangling_1 <- vis_wrangling %>%
  right_join(all) %>%
  mutate(ARUs = replace_na(ARUs, 0)) 


## making ggplot
ggplot(data = vis_wrangling_1) +
  geom_line(aes(x = md, y = ARUs, colour = factor(year)), 
            linewidth = 1.5) +
  scale_colour_brewer(palette = "Accent") +
  scale_x_date(date_labels = "%b", date_breaks = "1 month") +
  theme_bw() +
  labs(x = "Day of a year", y = "No. of active ARU") +
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 12),
        legend.title = element_blank(),
        legend.text = element_text(size = 14),
        legend.position = c(0.1, 0.82),
        axis.title.y = element_text(margin = margin(t = 0, r = 12, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 5, r = 0, b = 0, l = 0)))

## save the plot
ggsave(filename = here("docs", "figures", "ARU_number_per_day.png"),
       height = 10,
       width = 20,
       units = "cm",
       dpi = 300)



# days with active ARUs for each of the site  -----------------------------

map_wrangling <- effort_eval_1 %>%
  group_by(site) %>%
  summarize(days = n_distinct(datetime %>% date()))
  
## load map files: site locations, JPRF grid, and North grid, all in WGS84
sites <- read_csv(here("data", "map", "JPRF_all_sites.csv")) %>%
  left_join(map_wrangling, by = join_by(Name == site)) %>%
  st_as_sf(coords = c("X", "Y"), crs = st_crs(4326)) %>%
  vect() 

grid_JPRF <- vect(here("data", "map", "JPRF Grid")) %>%
  project(sites)

grid_North <- vect(here("data", "map", "North Grid Jprf")) %>%
  project(sites) %>%
  drop_na(site)
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  


