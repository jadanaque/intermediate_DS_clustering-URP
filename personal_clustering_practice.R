library(readr)
library(dplyr)
library(magrittr)

# Read and pre-process the data----
carsb <- read_tsv("cars.tab")

cars_use <- carsb[, -c(1, 2)]
medians <- sapply(cars_use, median)
mads <- sapply(cars_use, mad)
cars_use <- scale(cars_use, center = medians, scale = mads)

# Hierarchical Clustering ----
cars_hclust <- hclust(dist(cars_use))

# Dendogram
plot(cars_hclust, carsb$Car, main = "Default from hclust")

# Size of clusters
groups_3 <- cutree(cars_hclust, 3)  # Cluster memberships
table(groups_3)  # Sizes

counts <- sapply(2:6, function(ncl){table(cutree(cars_hclust, ncl))})  # The same (sizes) for different number of clusters
names(counts) <- 2:6
counts

# Name of cars in a cluster
carsb$Car[groups_3 == 1]

sapply(unique(groups_3), function(cl){carsb$Car[groups_3 == cl]})

groups_4 <- cutree(cars_hclust, 4)
sapply(unique(groups_4), function(cl)carsb$Car[groups_4 == cl])

# Checking if observations naturally group themselves in accord with an already measured variable.
table(groups_3, carsb$Country)

# Characterizing clusters
aggregate(cars_use, list(groups_3), median)

as_tibble(cars_use) %>%
  bind_cols(tibble(cluster = groups_3)) %>%
  group_by(cluster) %>%
  summarise_all(median)

aggregate(carsb[, -c(1, 2)], list(groups_3), median)

a3 <- aggregate(carsb[, -c(1, 2)], list(groups_3), median)
data.frame(Cluster = a3$Group.1, Size = as.vector(table(groups_3)), a3[, -1])
tibble(Cluster = a3$Group.1, Size = as.vector(table(groups_3))) %>% bind_cols(a3[, -1])  # The same

a4 <- aggregate(carsb[, -c(1, 2)], list(groups_4), median)
data.frame(Cluster = a4$Group.1, Size = as.vector(table(groups_4)), a4[, -1])

# Partitioning around medoids ----