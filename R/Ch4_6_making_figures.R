

# library -----------------------------------------------------------------

library(tidyverse)
library(here)
library(patchwork)
library(iNEXT)


# data import -------------------------------------------------------------

load(here("data", "iNEXT_model", "Hills.rda"))



# ARU days that one specie got observed in each site ----------------------

Hills_vis <- Hills %>%
  select(-incidence_freq) %>%
  unnest(species_matrix) %>%
  
  # Rank species by the number of sites they appear in
  group_by(common_name) %>%
  mutate(species_presence = n_distinct(site),
         species_ARU_days_total = sum(species_ARU_days, na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(desc(species_presence), desc(species_ARU_days_total)) %>%
  mutate(common_name = factor(common_name, 
                              levels = unique(common_name))) %>%
  
  # plot
  ggplot(aes(x = site, y = common_name, fill = species_ARU_days)) +
  geom_tile() + 
  
  # fine tune
  scale_x_discrete(guide = guide_axis(n.dodge = 3), position = "top") +
  scale_fill_distiller(palette = "YlOrRd", direction = 1,
                       guide = guide_colorbar(barwidth = 40, barheight = 0.8)) +
  
  # set theme
  theme_bw() +
  labs(x = "Site", y = NULL, fill = "Site by species ARU days") +
  theme(legend.position = "bottom",
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 12),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        
        axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 0, r = 0, b = 10, l = 0)),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 12))

Hills_vis 

ggsave(plot = Hills_vis,
       filename = here("docs", "figures", "Hills_vis.png"),
       width = 38,
       height = 25,
       units = "cm",
       dpi = 300)




# iNEXT curves by ARU days ------------------------------------------------

Hills_curves <- Hills %>%
  
  # group the data by the number of ARU days
  mutate(survey_effort = case_when(site_ARU_days < 90 ~ "Low",
                                   site_ARU_days >= 90 & site_ARU_days <= 120 ~ "Medium",
                                   site_ARU_days > 120 ~ "High")) %>%
  group_nest(survey_effort) %>%
  
  # plot the curves based on nested data
  mutate(plot = map(.x = data, 
                    .f =~ .x %>% 
                      select(site, incidence_freq) %>%
                      deframe() %>%
                      iNEXT(., q = 0, datatype = "incidence_freq") %>%
                      
                      # iNEXT result shown by plot
                      ggiNEXT(., type = 1, se = FALSE) +
                      labs(x = "ARU days", y = "No. species") +
                      scale_color_manual(values = rep("grey40", 59)) +  # set line color for 59 sites
                      scale_fill_manual(values = rep("grey80", 59)) +     # set ribbon fill for 59 sites
                      
                      # set theme
                      theme_bw() +
                      theme(legend.position = "none",
                            axis.title = element_text(size = 16),
                            axis.text = element_text(size = 12),
                            axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
                            axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0)))
                    
  ))
  

# low
Hills_curves_low <- Hills_curves$plot[[2]] + 
  geom_text(label = "0 - 90 days", x = 130, y = 5, size = 6)
Hills_curves_low$layers[[1]]$aes_params$size <- 0.2
Hills_curves_low$layers[[2]]$aes_params$alpha <- 0.7

# medium
Hills_curves_medium <- Hills_curves$plot[[3]] + 
  geom_text(label = "90 - 120 days", x = 120, y = 5, size = 6)
Hills_curves_medium$layers[[1]]$aes_params$size <- 0.2
Hills_curves_medium$layers[[2]]$aes_params$alpha <- 0.7

# high
Hills_curves_high <- Hills_curves$plot[[1]] + 
  geom_text(label = "> 120 days", x = 130, y = 5, size = 6)
Hills_curves_high$layers[[1]]$aes_params$size <- 0.2
Hills_curves_high$layers[[2]]$aes_params$alpha <- 0.7


Hills_curves_low
Hills_curves_medium
Hills_curves_high

Hills_curves_combined <- 
  Hills_curves_low + Hills_curves_medium + Hills_curves_high +
  plot_annotation(tag_levels = "A") +
  plot_layout(axes = "collect") & 
  theme(plot.tag = element_text(size = 16)) &
  ylim(1, 40) &
  xlim(0, 200) 

Hills_curves_combined

ggsave(plot = Hills_curves_combined,
       filename = here("docs", "figures", "Hills_curves_combined.png"),
       width = 25,
       height = 12,
       units = "cm",
       dpi = 300)






