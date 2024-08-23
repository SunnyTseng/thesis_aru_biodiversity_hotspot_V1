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
library(rnaturalearth) ## to get the outline of Canada

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

effort_eval_1 <- effort_eval %>%
  filter(datetime %within% interval(ymd("2020-05-01"), ymd("2020-07-31")) | 
         datetime %within% interval(ymd("2021-05-01"), ymd("2021-07-31")) | 
         datetime %within% interval(ymd("2022-05-01"), ymd("2022-07-31"))) %>%
  filter(datetime %>% hour() >= 4 & datetime %>% hour() <= 7) %>%
  filter(size >= 5760000 & size <= 5760500)

save(effort_eval_1, file = here("effort_site_date.RData"))


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
effort_change <- ggplot(data = vis_wrangling_1) +
  geom_line(aes(x = md, y = ARUs, colour = factor(year)), 
            linewidth = 1.5) +
  scale_colour_brewer(palette = "Accent") +
  scale_x_date(date_labels = "%b", date_breaks = "1 month") +
  theme_bw() +
  labs(x = "Day of a year", y = "No. of ARU") +
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 12),
        legend.title = element_blank(),
        legend.text = element_text(size = 14),
        legend.position = c(0.08, 0.75),
        axis.title.y = element_text(margin = margin(t = 0, r = 0, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 5, r = 0, b = 0, l = 0)))


# days with active ARUs for each of the site  -----------------------------

map_wrangling <- effort_eval_1 %>%
  group_by(site) %>%
  summarize(days = n_distinct(datetime %>% date()))
  
## load map files: site locations, JPRF grid, and North grid, all in WGS84
sites <- read_csv(here("data", "map", "JPRF_all_sites.csv")) %>%
  left_join(map_wrangling, by = join_by(Name == site)) %>%
  mutate(days = replace_na(days, 0)) %>%
  st_as_sf(coords = c("X", "Y"), crs = st_crs(4326)) %>%
  vect() 

grid_JPRF <- vect(here("data", "map", "JPRF Grid")) %>%
  project(sites)

grid_North <- vect(here("data", "map", "North Grid Jprf")) %>%
  project(sites) %>%
  drop_na(site)
  
## get base map

extent <- sites %>% 
  ## add a buffer around the actual camera points so the the base map extends beyond them (in meters)
  buffer(3000) %>% 
  ext() %>%
  as.vector()

register_stadiamaps("483bb38f-7ba0-4c89-b2f5-fc8310057ab4", write = FALSE)

basemap <- get_stadiamap(bbox = c(left = extent[[1]] - 0.1, 
                                  right = extent[[2]],
                                  bottom = extent[[3]], 
                                  top = extent[[4]]), 
                         maptype = "alidade_smooth")

## main map is here
ARU_map <- ggmap(basemap) + 
  #geom_spatvector(data = grid_JPRF, inherit.aes = F, alpha = 0.1, fill = "lightsteelblue2") +
  #geom_spatvector(data = grid_North, inherit.aes = F, alpha = 0.1, fill = "lightsteelblue2") +
  geom_spatvector(inherit.aes = F, data = sites, 
                  alpha = 0.8, colour = "deepskyblue4", stroke = 0, shape = 16) +
  geom_spatvector(inherit.aes = F, data = sites, aes(size = days), 
                  alpha = 0.3, colour = "brown1", stroke = 0, shape = 16) +
  scale_size_area(max_size = 14) +
  labs(size = "ARU days") +
  ## add scale bar
  annotation_scale(location = "bl", 
                   width_hint = 0.4) +
  ## add north arrow:
  annotation_north_arrow(style = north_arrow_fancy_orienteering, 
                         location = "bl",
                         pad_x = unit(0.0, "in"), pad_y = unit(0.2, "in"),
                         height = unit(2, "cm"),
                         width = unit(2, "cm")) +
  #theme_void() +
  theme(legend.position = c(0.1, 0.41),
        legend.title = element_text(hjust = 0.5),
        legend.text = element_text(size = 10),
        legend.margin = margin(5, 5, 5, 5),
        axis.title = element_blank(),
        axis.text = element_text(size = 12))


## inset map here
## boundaries for Canada provinces
canada <- ne_states(country = "Canada", returnclass = "sf") %>%
  st_transform(crs = 3347)

## coordinates for Fort St. James
fort_st_james <- data.frame(lon = -124.2545, lat = 54.4692) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  st_transform(crs = 3347)

inset <- ggplot(data = canada) +
  geom_sf(fill = "cornflowerblue", colour = "black", alpha = 0.7) +
  geom_sf(data = fort_st_james, color = "red", size = 3, shape = 18) +
  theme_void() 
#theme(panel.background = element_rect(fill = "transparent", colour="antiquewhite4"))





# combining effort plots --------------------------------------------------

combined <- plot_grid(ARU_map, effort_change, 
          labels = "AUTO", label_size = 18,
          ncol = 1, rel_heights = c(2,1), align = "v")

ggdraw() +
  draw_plot(combined, x = 0, y = 0) +
  draw_plot(inset, x = 0.12, y = 0.68, width = 0.32, height = 0.32) 

ggsave(filename = here("docs", "figures", "ARU_effort.jpg"),
       width = 22,
       height = 22,
       units = "cm",
       dpi = 300)

  
  
  
  
  
  
  
  


