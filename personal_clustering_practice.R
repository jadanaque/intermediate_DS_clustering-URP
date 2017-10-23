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
library(cluster)
cars_pam <- pam(dist(cars_use), 3)  # cars_dist <- dist(cars_use)

names(cars_pam)
table(groups_3, cars_pam$clustering)

# Which one is differing with our hclust model?
carsb$Car[groups_3 != cars_pam$clustering]

# Which cars are representative (typical) of their clusters? Medoids
carsb$Car[cars_pam$id.med]
carsb[cars_pam$id.med, ]

# Silhouette plot
plot(cars_pam)  # for me, best model: pam with 3 clusters

# Now, one more pam cluster analysis using 4 clusters
cars_pam4 <- pam(dist(cars_use), 4)
plot(cars_pam4)

# To plot the silhouette for a different cluster analysis, use the silhouette function
plot(silhouette(cutree(cars_hclust, 4), dist(cars_use)))


# Checking the optimal number of clusters
wss <- (nrow(cars_use)-1)*sum(apply(cars_use,2,var))  # Total_SS

medias1 <- apply(cars_use, 2, mean)
sum(apply(cars_use, 1, function(obs){sum((obs - medias1)^2)}))  # Should be the same: Total_SS

# For each number of clusters, compute kmeans
for (i in 2:15) wss[i] <- kmeans(cars_use, centers=i)$tot.withinss  # all the sum can be replaced by 'kmeans(.)$tot.withinss
plot(1:15, wss, type="b",
     xlab="Nummero de Clusters",
     ylab="Suma de cuadrados within", col = 4)

# 3 appears to be an optimal number of clusters

# Let's try a kmeans cluster analysis using 3 clusters:
cars_kmeans <- kmeans(cars_use, centers = 3, iter.max=20)
plot(silhouette(cars_kmeans$cluster, dist(cars_use)))

table(cars_kmeans$cluster)

table(groups_3, cars_kmeans$cluster)
table(cars_pam$clustering, cars_kmeans$cluster)
