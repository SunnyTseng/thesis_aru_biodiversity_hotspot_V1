###
### Name: Checking the JPRF ARU effort
### 
### Author: Sunny Tseng
### Date: 2024 Aug 20
###


# library -----------------------------------------------------------------

library(tidyverse)
library(here)
library(lubridate)
library(ggh4x)


# check efforts -----------------------------------------------------------

# dir <- "E:/Audio/2023_passerine"
# 
# effort_info <- tibble()
# sites <- list.files(dir)
# for (site in sites) {
#   
#   files <- list.files(file.path(dir, site), pattern = 'WAV')
#   for (file in files) {
#     
#     size <- file.info(file.path(dir, site, file))$size
#     year <- str_sub(file, start = 1, end = 4)
#     month <- str_sub(file, start = 5, end = 6)
#     day <- str_sub(file, start = 7, end = 8)
#     hour <- str_sub(file, start = 10, end = 11)
#     minute <- str_sub(file, start = 12, end = 13)
#     
#     temp <- c(site, year, month, day, hour, minute, size)
#     effort_info <- rbind(effort_info, temp)
#   }
# }
# names(effort_info) <- c("site", "year", "month", "day", "hour", "minute", "size")
# write_csv(effort_info, "E:/Audio/2023_passerine_effort.csv")

effort_list <- c("2020_passerine_effort.csv", 
                 "2021_passerine_effort.csv",
                 "2022_passerine_effort.csv")

effort_all <- tibble()
for (file in effort_list) {
  effort_temp <- read_csv(here("data", "JPRF_aru_effort", file))
  effort_all <- rbind(effort_all, effort_temp)
}

effort_all_1 <- effort_all %>%
  unite(col = date, year, month, day, remove = FALSE) %>%
  mutate(date = ymd(date)) %>%
  mutate(month = as.numeric(month),
         day = as.numeric(day),
         hour = as.numeric(hour),
         minute = as.numeric(minute)) %>%
  filter(size >= 5760000 & size <= 5760500,
         year >= 2020 & year <= 2022,
         month >= 5 & month <= 7,
         hour %in% c(4, 5, 6),
         minute %in% seq(0, 60, by = 5)) 

# write_csv(effort_all_1 %>% 
#             group_nest(site, date) %>%
#             select(site, date),
#            here("data", "JPRF_aru_effort", "2020_2022_passerine_effort_filter.csv"))


effort_break <- effort_all_1 %>%
  group_by(year) %>%
  summarize(start = min(date),
            end = max(date))

effort_all_2 <- effort_all_1 %>%
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




