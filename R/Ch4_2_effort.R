###
### Name: Checking the JPRF ARU effort
### Author: Sunny Tseng
### Date: 2024 Aug 20
###


# library -----------------------------------------------------------------

library(tidyverse)
library(here)
library(lubridate)
library(ggh4x)


# functions ---------------------------------------------------------------

get_effort <- function(dir){
  effort_info <- tibble()
  files <- list.files(path = dir, 
                      pattern = 'WAV', 
                      recursive = TRUE, 
                      full.names = TRUE)
  
  effort_info <- tibble(file = files) %>%
    mutate(site = file %>% str_split_i(pattern = "/", 4),
           datetime = file %>% str_split_i(pattern = "/", 5) %>% ymd_hms(),
           size = file %>% file.info() %>% pull(size))
  
  return(effort_info)
}


# check efforts -----------------------------------------------------------

effort_eval <- c("E:/Audio/2020_passerine", 
                 "E:/Audio/2021_passerine",
                 "E:/Audio/2022_passerine") %>%
  map_df(~ get_effort(dir = .))

save(effort_eval, file = here("effort_eval.RData"))

effort_eval_1 <- effort_eval %>%
  filter(datetime %within% interval(ymd("2020-05-01"), ymd("2020-07-31")) | 
           datetime %within% interval(ymd("2021-05-01"), ymd("2021-07-31")) | 
           datetime %within% interval(ymd("2022-05-01"), ymd("2022-07-31"))) %>%
  filter(datetime %>% hour() >= 4 & datetime %>% hour() <= 7) %>%
  filter(size >= 5760000 & size <= 5760500)



# visualization of the effort data ----------------------------------------

effort_vis <- effort_eval_1 %>%
  
  
  
  group_nest(date) %>%
  mutate(ARUs = map_dbl(.x = data, .f =~ .x %>% pull(site) %>% n_distinct()),
         year = map_dbl(.x = data, .f =~ .x %>% pull(year) %>% unique()),
         yday = yday(date)) %>%
  ggplot(aes(x = yday, y = ARUs)) +
    geom_line() +
    geom_point(shape = 16, alpha = 0.5) +
    # geom_bar(stat = "identity") +
    facet_grid(rows = vars(year)) +
    theme_bw() +
    labs(x = "Day of a year", y = "# of ARU") +
  scale_y_continuous(breaks = c(10, 40)) +
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 12),
        legend.title = element_blank(),
        legend.text = element_text(size = 14),
        legend.position = c(0.8, 0.2), 
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)))

ggsave(plot = effort_all_2,
       filename = here("docs", "figures", "effort_all_2.png"),
       height = 10,
       width = 20,
       units = "cm",
       dpi = 300)




