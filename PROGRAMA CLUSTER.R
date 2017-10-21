rm(list=ls())
# Autor: Lic. Jose Cardenas

#GASTOS EN PUBLICIDAD
datos=read.table("DATAS/MERCADO.txt",header=T)
datos
attach(datos)
plot(PUBLICIDAD,VENTAS)
text(PUBLICIDAD+1, VENTAS, labels = EMPRESA, cex= 0.7, offset = 10)  # 'offset' option is only necessary when 'pos' is specified (1, 2, 3 or 4)
MdistE=dist(datos[,2:3],diag=T,upper=T)
MdistE


MdistMa=dist(datos[,2:3],method="manhattan",diag=T,upper=T)
MdistMa
MdistMi=dist(datos[,2:3],method="minkowski",diag=T,upper=T)
MdistMi

#ACTIVOS Y N?MERO DE TRABAJADORES
datos2=read.table("DATAS/ACTIVOS TRABAJADORES.txt",header=T)
datos2
MdistE2=dist(datos2[,2:3],diag=T,upper=T)
MdistE2

########### librerias ############################

# install.packages("cluster")
# install.packages("fpc")
# install.packages("mclust")
# install.packages("dbscan")
library("cluster")
library("fpc")
library("mclust")
library("dbscan")
library(readxl)

#preparar los datos: normalizar y estandarizar

carros <- read.csv("DATAS/carros2011imputado.csv", 
                 header=TRUE, sep=";", dec=",")

str(carros)
numericos <- sapply(carros, is.numeric)
carrosnum<- carros[ , numericos]
carroslabel<-paste(carros$fabricante,carros$modelo)

#estandarizar 
carrosnormal <- scale(carrosnum)
#conocer/guardar las medias y desviaciones
medias<-attr(carrosnormal,"scaled:center")
desviaciones<-attr(carrosnormal,"scaled:scale")

carrosnormal<-carrosnormal[,-1]
#métricas de distancia: usar dist() o libreria philentropy
ejemploeuclid<-dist(carrosnormal[2:3,],method="euclidean")
carrosdist<-dist(carrosnormal,method="euclidean")
ejemploeuclid/ncol(carrosnormal)
#defini el método entonces hay que usar:
###para k-medias  kmeans de stats

carroskmcluster<-kmeans(carrosnormal,centers=4,iter.max=20)

#a que cluster qued??
carroskmcluster$cluster

#centros de cluster- para jugar y mirar e interpretar
carroskmcluster$centers
tab<-as.data.frame(t(carroskmcluster$centers))

carroskmcluster$size

carroskmcluster$iter

clusplot(carros,carroskmcluster$cluster, color=TRUE)
#row.names(carros) <- carroslabel
#clusplot(carros,carroskmcluster$cluster, color=TRUE, labels = 2)

###seleccionar n?mero de clusters

#calcula la suma total de cuadrados
wss <- (nrow(carrosnormal)-1)*sum(apply(carrosnormal,2,var))
#la calcula por clusters
for (i in 2:15) wss[i] <- sum(kmeans(carrosnormal,
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Nummero de Clusters",
     ylab="Suma de cuadrados within")  # Se seleccionan 4 porque de ese punto en adelante, la suma de cuadrados
                                       # ya no varía mucho.

## Cuando se tiene un volumen de datos grande, usar el paquete biganalytics
install.packages("biganalytics")
library(biganalytics)
# Usar la funci?n 'bigkmeans()' si se tiene un volumen de datos muy grande