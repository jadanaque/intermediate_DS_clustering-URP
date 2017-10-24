library(cluster)

# Carga de datos
seguros_df <- read.csv("DATAS/SEGURO.csv", stringsAsFactors = FALSE)

dim(seguros_df)
sapply(seguros_df, class)

seguros_df <- seguros_df[, sapply(seguros_df, is.numeric)]  # sólo valores númericos

seguros_normal <- scale(seguros_df)
seguros_km <- kmeans(seguros_normal, centers = 30, iter.max = 20)

seguros_km$cluster
seguros_km$centers

clusplot(seguros_df, seguros_km$cluster, color = TRUE)
#clusplot(seguros_normal, seguros_km$cluster, color = TRUE)

# Probar número óptimo de clusters
wss <- (nrow(seguros_normal) - 1) * sum(apply(seguros_normal, 2, var))  # Total_SS

for (i in 2:40){
  wss[i] <- kmeans(seguros_normal, centers = i, iter.max = 20)$tot.withinss
}

plot(1:40, wss, type = "b",
     xlab = "Número de Clusters",
     ylab = "suma de cuadrados within")  # Alrededor de 12 clusters es el óptimo


