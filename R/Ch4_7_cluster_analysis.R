### Author: Sunny Tseng
### Date: 2025 09 13
### Purpose: Which bird species occur on the same sites? 



# library -----------------------------------------------------------------

library(tidyverse)
library(here)
library(factoextra)
library(cluster)
library(ggplot2)



# load data ---------------------------------------------------------------

load(here("data", "iNEXT_model", "Hills.rda"))

data <- Hills %>%
  
  # arrange the columns
  select(-incidence_freq) %>%
  unnest(species_matrix) %>%
  
  # make the relative presence species
  mutate(relative_presence_perc = species_ARU_days/site_ARU_days) %>%
  select(-species_ARU_days, -site_ARU_days) %>%
  
  # shape the data
  pivot_wider(names_from = site, 
              values_from = relative_presence_perc,
              values_fill = 0) 


data_cluster <- data %>%
  as.data.frame() %>%
  column_to_rownames(common_name)



# find number of cluster --------------------------------------------------

# total within sum of squares
fviz_nbclust(data_cluster, kmeans, method = "wss")
# looks like 3 or 6 clusters

# gap statistic
gap_stat <- clusGap(data_cluster, FUN = kmeans, nstart = 25, K.max = 10, B = 50)
fviz_gap_stat(gap_stat)
# looks like 6 clusters could be the best




# k-means clustering ------------------------------------------------------
set.seed(1)

km <- kmeans(data_cluster, centers = 3)

species_cluster <- fviz_cluster(km, data = data_cluster, 
             repel = TRUE,
             show.clust.cent = FALSE,
             shape = 16,
             main = NULL,
             xlab = "PC1",
             ylab = "PC2") +
  
  xlim(-18, 12) +
  ylim(-6, 7) +
  scale_color_brewer(palette = "Dark2") +
  scale_fill_brewer(palette = "Dark2") +
  
  theme_bw() +
  theme(axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.position = "none")


ggsave(plot = species_cluster,
       filename = here("docs", "figures", "species_cluster.png"),
       width = 25,
       height = 12,
       units = "cm",
       dpi = 300)
