# Tarea: Trabajar kmeans con uno de los datasets otorgados en clase
# Author: Javier Adanaque

library(cluster)

# Carga de datos
iris_df <- read.csv("DATAS/iris.csv", dec = ",")

dim(iris_df)
str(iris_df)

#  K-means con el dataset "iris" otorgado en clase
set.seed(123)
iris_km <- kmeans(iris_df[, 1:4], centers = 3, iter.max = 20, nstart = 20)

iris_km$centers

# Comparación con grupos verdaderos (Especies)
table(iris_df$Species, iris_km$cluster)  # 16 mal clasificados: 16/150 = 0.107

## Probando con PAM
iris_dist <- dist(iris_df[, 1:4], method = "manhattan")  # Manhattan arroja mejores resultados después de intentar con otros métodos

iris_pam <- pam(iris_dist, 3)

table(iris_df$Species, iris_pam$clustering)  # 15 mal clasificados: 15 / 150 = 0.1

## Probando con Hierarchical Clustering
iris_hclust <- hclust(iris_dist, method = "average")

g3 <- cutree(iris_hclust, 3)

table(iris_df$Species, g3)  # 15 mal clasificados: 15 / 150 = 0.1



# Ahora normalizado
# iris_normal <- scale(iris_df[, 1:4])  # Todos están en la misma, así que no estandarizo
# iris_normal_km <- kmeans(iris_normal, centers = 3, iter.max = 20)
# iris_normal_km$cluster
# table(iris_df$Species, iris_normal_km$cluster)
