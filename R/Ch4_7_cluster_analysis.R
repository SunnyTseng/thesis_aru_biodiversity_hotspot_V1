### Author: Sunny Tseng
### Date: 2025 09 13
### Purpose: Which bird species occur on the same sites? 


library(tidyverse)
library(here)


load(here("data", "iNEXT_model", "Hills.rda"))

test <- Hills %>%
  
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


# Again, subset the data to retain just the bird info.
justx <- test[,-1] 
#rownames(justx) <- test$common_name
n <- nrow(justx)

## calculate the within sums of squares (wss) for 1 to 6 clusters
wss <- rep(0, 6)
# calculate the wss when only having 1 cluster 
wss[1] <- (n - 1) * sum(sapply(justx, var)) 
# calculate wss when having 2 to 6 clusters for k-means
for (i in 2:6)
  wss[i] <- sum(kmeans(justx, centers = i)$withinss)

# print out the wss for each scenario
plot(1:6, wss, type = "b", xlab = "Number of groups",
     ylab = "Within groups sum of squares")



## 3 groups
(threegroups <- kmeans(justx, centers = 3))

str(threegroups) # includes the cluster no, etc.
Keep3<-data.frame(justx,as.factor(threegroups$cluster) )
head(Keep3) # cluster number is added to the data.
summary(Keep3) # most are in Cluster 1 here.

## 4 groups
(fourgroups <- kmeans(justx, centers = 4))

str(fourgroups) # includes the cluster no, etc.
Keep4<-data.frame(justx,as.factor(fourgroups$cluster) )
head(Keep4) # cluster number is added to the data.
summary(Keep4)

## 
colorlow <- rainbow(3)[threegroups$cluster]
colorup <- rainbow(4)[fourgroups$cluster]
panel.up <- function(x, y, ...)
{ # function to produce scatterplot in pairs()
  usr <- par("usr") # save par() values
  on.exit(par(usr)) # set par() at end
  par(usr = c(max(y), min(y), max(x), min(x))) # set bounds
  points(y, x, col = colorup, pch = 16) # plot data points
}
pairs(justx, pch = 16, gap = 0, xaxt = "n", yaxt = "n",
      col = colorlow, upper.panel = panel.up)

##
dim(Keep4)
summary(Keep4)

cluster1<-Keep4[(Keep4$as.factor.fourgroups.cluster==1),]
cluster2<-Keep4[(Keep4$as.factor.fourgroups.cluster==2),]
cluster3<-Keep4[(Keep4$as.factor.fourgroups.cluster==3),]
cluster4<-Keep4[(Keep4$as.factor.fourgroups.cluster==4),]

dim(cluster1)
dim(cluster2)
dim(cluster3)
dim(cluster4)

rownames(cluster1) # what birds in cluster 1? 
rownames(cluster2)
rownames(cluster3)
rownames(cluster4)
