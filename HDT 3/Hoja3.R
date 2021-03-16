###Renato Estrada

installed.packages("ggplot2")
library("dplyr")
library("cluster")
library("e1071")
library("fpc")
library("mclust")
library("factoextra")
library("NbClust")
library("tidyverse")
library("plotly")

#cargar datos 
datos<-read.csv("train.csv")
?index
datos[is.na(datos)] = "NO"
?c
barplot(
  table()
)


datos[datos=="NO",]

###Precio medio por cantidad de habitaciones
bedroom <- group_by(datos, BedroomAbvGr)
summarise(bedroom, mean_sell_for_bedroom = mean(SalePrice))
total_bed_sales <- table(datos$BedroomAbvGr)
total_bed_sales2 <- aggregate(datos[,81],list(datos$BedroomAbvGr),mean)
total_bed_mean <- total_bed_sales2$x
names(total_bed_mean) <- total_bed_sales2$Group.1

barplot(
  total_bed_mean, 
  main="Precio medio por cantidad de habitaciones",
  xlab = "No, habitaciones",
  ylab = "Precio medio",
  ylim = c(0,250000),
  col = "darkgreen"
)

###Precio medio por cantidad de total de lo cuartos 
total_room_sales <- aggregate(datos[,81],list(datos$TotRmsAbvGrd),mean)
total_room_mean <- total_room_sales$x
names(total_room_mean) <- total_room_sales$Group.1

barplot(
  total_room_mean, 
  main="Precio medio por cantidad de habitaciones",
  xlab = "No. habitaciones totales",
  ylab = "Precio medio",
  ylim = c(0,350000),
  col = "purple"
)

### Numero de habitaciones mas rentalbles 
rentabilidad_hab <- table(datos$BedroomAbvGr)
barplot(
  rentabilidad_hab,
  main="Cantidad de ventas totales respecto a la cantidad de habitaciones",
  xlab = "No. habitaciones",
  ylab = "Ventas totales",
  col = "red"
)  

### Numero de cuartos mas rentalbles 
rentabilidad_cuartos <- table(datos$TotRmsAbvGrd)
barplot(
  rentabilidad_cuartos,
  main="Cantidad de ventas totales respecto a la cantidad de cuartos totales",
  xlab = "No. habitaciones totales",
  ylab = "Ventas totales",
  col = "lightblue"
)  

### Relacion entre la cantidad de cuartos y habitaciones de una habitacion
relacion_hab_cuarto <- table(datos$BedroomAbvGr, datos$TotRmsAbvGrd, datos$SalePrice)
view(relacion_hab_cuarto)
plot(
  datos$BedroomAbvGr, datos$TotRmsAbvGrd
)


### relacion entre la cantidad de habitaciones y si estas eran cuartos con el precio de la casa

total_bedycu_sales <- aggregate(datos[,81],list(datos$TotRmsAbvGrd, datos$BedroomAbvGr),mean)
names(total_bedycu_sales) <- c("Habitaciones", "Cuartos", "Precio medio")

barplot(
  as.matrix(total_bedycu_sales),
  main="Cantidad de ventas totales respecto a la cantidad de cuartos totales",
  xlab = "No. habitaciones totales",
  ylab = "Ventas totales",
  col = "lightblue"
) 

###¿La modernidad de una casa  y sus utilidades afecta a su precio? 
utilities_sales <- aggregate(datos[,81],list(datos$Utilities),mean)
total_utilities_sales <- utilities_sales$x
names(total_utilities_sales) <- utilities_sales$Group.1

barplot(
  total_utilities_sales, 
  main="Precio medio por las utilidades de la casa",
  xlab = "Utilidades que ofrece",
  ylab = "Precio medio",
  ylim = c(0,200000),
  col = "darkgreen"
)

###Calefaccion
Heating_sales <- aggregate(datos[,81],list(datos$Heating),mean)
total_Heating_sales <- Heating_sales$x
names(total_Heating_sales) <- Heating_sales$Group.1

barplot(
  total_Heating_sales, 
  main="Precio medio por las utilidades de la casa",
  xlab = "Utilidades que ofrece",
  ylab = "Precio medio",
  ylim = c(0,200000),
  col = "darkgreen"
)

### sistema electrico

Electrical_sales <- aggregate(datos[,81],list(datos$Electrical),mean)
total_Electrical_sales <- Electrical_sales$x
names(total_Electrical_sales) <- Electrical_sales$Group.1

barplot(
  total_Electrical_sales, 
  main="Precio medio por el sistema electrico de la casa",
  xlab = "Sistema que ofrece",
  ylab = "Precio medio",
  ylim = c(0,200000),
  col = "darkgreen"
)

###Aire acondicionado
Air_sales <- aggregate(datos[,81],list(datos$CentralAir),mean)
total_Air_sales <- Air_sales$x
names(total_Air_sales) <- Air_sales$Group.1

barplot(
  total_Air_sales, 
  main="Precio medio por casas que tengan sistema de Aire",
  xlab = "Aire acondicionado",
  ylab = "Precio medio",
  ylim = c(0,200000),
  col = "darkgreen"
)

###Calefaccion

Heating_sales <- aggregate(datos[,81],list(datos$Heating),mean)
total_Heating_sales <- Heating_sales$x
names(total_Heating_sales) <- Heating_sales$Group.1

barplot(
  total_Heating_sales, 
  main="Precio medio por el sistema de calefaccion de la casa",
  xlab = "Sistema de calefaccion",
  ylab = "Precio medio",
  ylim = c(0,200000),
  col = "darkgreen"
)

###Clustering
datosC<-select(datos, LotArea, TotalBsmtSF, X1stFlrSF, X2ndFlrSF, WoodDeckSF, OpenPorchSF, EnclosedPorch, X3SsnPorch, ScreenPorch, PoolArea, MiscVal, SalePrice)
datosC <- as.matrix(scale(datosC))

fviz_nbclust(datosC, kmeans, method = "gap_stat")
fviz_nbclust(datosC, kmeans, method = "silhouette")

###kmeans 
km<-kmeans(datosC, 3, iter.max = 100)
fviz_cluster(km, data = datosC, frame.type = "convex")
?fviz_cluster
print(km)
clusterM <- aggregate(datos, by=list(cluster=km$cluster), mean)
table(clusterM$SalePrice)
print(clusterM)
km$centers

rawData$group <- km$cluster
groupOneData <- rawData[rawData$group == 1,]
groupTwoData <- rawData[rawData$group == 2,]
groupThreeData <- rawData[rawData$group == 3,]

?unscale
MyDeScaledData<-as.data.frame(Map(descale,km,minvec[names(km)],maxvec[names(km)]))
