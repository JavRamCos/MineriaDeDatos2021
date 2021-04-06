install.packages("ggplot2")
library("readxl")
library("ggplot2")
library("dplyr")
library("cluster")
library("e1071")
library("fpc")
library("mclust")
library("factoextra")
library("NbClust")
library("tidyverse")
library("plotly")

###Matrimonios por edades de la madre
Matrimonio2009 <- read_excel(
  "Matrimonio2009.xls", 
   sheet = "Grupos de edad novio y novia", 
   range = "A2:B17", 
   col_types = c("text", "numeric")
)
Mat2009 <- Matrimonio2009[c(2,3,4,5,6,7,8,9,10,11,12,13,14),]
utilities_sales <- aggregate(Mat2009[,2],list(Mat2009$`Edad de la novia`),mean)
total_utilities_sales <- utilities_sales$Total
names(total_utilities_sales) <- utilities_sales$Group.1
barplot(
  total_utilities_sales, 
  main="Matrimonios totales por rango de edad 2009",
  xlab = "Edades",
  ylab = "No. Matrimonios",
  col = "blue",
)


   
Matrimonio2010 <- read_excel(
  "Matrimonio2010.xls", 
  sheet = "Grupos de edad novio y novia", 
  range = "A2:B17", 
  col_types = c("text", "numeric")
)
Mat2010 <- Matrimonio2010[c(2,3,4,5,6,7,8,9,10,11,12,13,14),]
utilities_sales <- aggregate(Mat2010[,2],list(Mat2010$`Edad de la novia`),mean)
total_utilities_sales <- utilities_sales$Total
names(total_utilities_sales) <- utilities_sales$Group.1
barplot(
  total_utilities_sales, 
  main="Matrimonios totales por rango de edad 2010",
  xlab = "Edades",
  ylab = "No. Matrimonios",
  col = "blue",
)
Matrimonio2011 <- read_excel(
  "Matrimonio2011.xls", 
  sheet = "Grupos de edad novio y novia", 
  range = "A2:B17", 
  col_types = c("text", "numeric")
)
Mat2011 <- Matrimonio2011[c(2,3,4,5,6,7,8,9,10,11,12,13,14),]
utilities_sales <- aggregate(Mat2011[,2],list(Mat2011$`Edad de la novia`),mean)
total_utilities_sales <- utilities_sales$Total
names(total_utilities_sales) <- utilities_sales$Group.1
barplot(
  total_utilities_sales, 
  main="Matrimonios totales por rango de edad 2011",
  xlab = "Edades",
  ylab = "No. Matrimonios",
  col = "blue",
)
Matrimonio2012 <- read_excel(
  "Matrimonio2012.xls", 
  sheet = "Grupos de edad novio y novia", 
  range = "A2:B17", 
  col_types = c("text", "numeric")
)
Mat2012 <- Matrimonio2012[c(2,3,4,5,6,7,8,9,10,11,12,13,14),]
utilities_sales <- aggregate(Mat2012[,2],list(Mat2012$`Edad de la novia`),mean)
total_utilities_sales <- utilities_sales$Total
names(total_utilities_sales) <- utilities_sales$Group.1
barplot(
  total_utilities_sales, 
  main="Matrimonios totales por rango de edad 2012",
  xlab = "Edades",
  ylab = "No. Matrimonios",
  col = "blue",
)
Matrimonio2013 <- read_excel(
  "Matrimonio2013.xls", 
  sheet = "Grupos de edad novio y novia", 
  range = "A2:B17", 
  col_types = c("text", "numeric")
)
Mat2013 <- Matrimonio2013[c(2,3,4,5,6,7,8,9,10,11,12,13,14),]
utilities_sales <- aggregate(Mat2013[,2],list(Mat2013$`Edad de la novia`),mean)
total_utilities_sales <- utilities_sales$Total
names(total_utilities_sales) <- utilities_sales$Group.1
barplot(
  total_utilities_sales, 
  main="Matrimonios totales por rango de edad 2013",
  xlab = "Edades",
  ylab = "No. Matrimonios",
  col = "blue",
)
Matrimonio2014 <- read_excel(
  "Matrimonio2014.xls", 
  sheet = "Grupos de edad novio y novia", 
  range = "A2:B17", 
  col_types = c("text", "numeric")
)
Mat2014 <- Matrimonio2014[c(2,3,4,5,6,7,8,9,10,11,12,13,14),]
utilities_sales <- aggregate(Mat2014[,2],list(Mat2014$`Edad de la novia`),mean)
total_utilities_sales <- utilities_sales$Total
names(total_utilities_sales) <- utilities_sales$Group.1
barplot(
  total_utilities_sales, 
  main="Matrimonios totales por rango de edad 2014",
  xlab = "Edades",
  ylab = "No. Matrimonios",
  col = "blue",
)
Matrimonio2015 <- read_excel(
  "Matrimonio2015.xls", 
  sheet = "Grupos de edad novio y novia", 
  range = "A2:B17", 
  col_types = c("text", "numeric")
)
Matrimonio2016 <- read_excel(
  "Matrimonio2016.xlsx", 
  sheet = "Grupos de edad novio y novia", 
  range = "A2:B17", 
  col_types = c("text", "numeric")
)
Matrimonio2017 <- read_excel(
  "Matrimonio2017.xlsx", 
  sheet = "Grupos de edad novio y novia", 
  range = "A2:B17", 
  col_types = c("text", "numeric")
)
Matrimonio2018 <- read_excel(
  "Matrimonio2018.xlsx", 
  sheet = "Grupos de edad novio y novia", 
  range = "A2:B17", 
  col_types = c("text", "numeric")
)
Matrimonio2019 <- read_excel(
  "Matrimonio2019.xlsx", 
  sheet = "Grupos de edad novio y novia", 
  range = "A2:B17", 
  col_types = c("text", "numeric")
)
Matrimonio2009<-na.omit(Matrimonio2009)
Matrimonio2010<-na.omit(Matrimonio2010)
Matrimonio2011<-na.omit(Matrimonio2011)
Matrimonio2012<-na.omit(Matrimonio2012)
Matrimonio2013<-na.omit(Matrimonio2013)
Matrimonio2014<-na.omit(Matrimonio2014)
Matrimonio2015<-na.omit(Matrimonio2015)
Matrimonio2016<-na.omit(Matrimonio2016)
Matrimonio2017<-na.omit(Matrimonio2017)
Matrimonio2018<-na.omit(Matrimonio2018)
Matrimonio2019<-na.omit(Matrimonio2019)

joven152009 <- Matrimonio2009$Total[3]
joven202009 <- Matrimonio2009$Total[4]
joven252009 <- Matrimonio2009$Total[5]
joven152010 <- Matrimonio2010$Total[3]
joven202010 <- Matrimonio2010$Total[4]
joven252010 <- Matrimonio2010$Total[5]
joven152011 <- Matrimonio2011$Total[3]
joven202011 <- Matrimonio2011$Total[4]
joven252011 <- Matrimonio2011$Total[5]
joven152012 <- Matrimonio2012$Total[3]
joven202012 <- Matrimonio2012$Total[4]
joven252012 <- Matrimonio2012$Total[5]
joven152013 <- Matrimonio2013$Total[3]
joven202013 <- Matrimonio2013$Total[4]
joven252013 <- Matrimonio2013$Total[5]
joven152014 <- Matrimonio2014$Total[3]
joven202014 <- Matrimonio2014$Total[4]
joven252014 <- Matrimonio2014$Total[5]
joven152015 <- Matrimonio2015$Total[3]
joven202015 <- Matrimonio2015$Total[4]
joven252015 <- Matrimonio2015$Total[5]
joven152016 <- Matrimonio2016$Total[3]
joven202016 <- Matrimonio2016$Total[4]
joven252016 <- Matrimonio2016$Total[5]
joven152017 <- Matrimonio2017$Total[3]
joven202017 <- Matrimonio2017$Total[4]
joven252017 <- Matrimonio2017$Total[5]
joven152018 <- Matrimonio2018$Total[3]
joven202018 <- Matrimonio2018$Total[4]
joven252018 <- Matrimonio2018$Total[5]
joven152019 <- Matrimonio2019$Total[3]
joven202019 <- Matrimonio2019$Total[4]
joven252019 <- Matrimonio2019$Total[5]
rango_15_19 <- c(joven152009, joven152010, joven152011, joven152012, joven152013, joven152014, joven152015, joven152016, joven152017, joven152018, joven152019)
rango_20_24 <- c(joven202009, joven202010, joven202011, joven202012, joven202013, joven202014, joven202015, joven202016, joven202017, joven202018, joven202019)
rango_24_30 <- c(joven252009, joven252010, joven252011, joven252012, joven252013, joven252014, joven252015, joven252016, joven252017, joven252018, joven252019)
Total_matrimonios <- data.frame(Year = years,
                                "15-19" = rango_15_19,
                                "20-24" = rango_20_24,
                                "25-29" = rango_24_30)

{plot(Total_matrimonios$X15.19,type="o",col="blue",ylim=c(0,30000),main="No. matriomonios respecto a la edad de la mujer 2009-2019",ylab="No. de matrimonios",xlab="Años",xaxt="n")
  lines(Total_matrimonios$X20.24,type="o",col="purple")
  lines(Total_matrimonios$X25.29,type="o",col="red")
  legend(8,5000,legend=c("15-19","20-24", "25-29"),
         col=c("Blue","purple", "red"),lty=1.2,cex=0.8)
  axis(1,1:11,years)}

###Cantidad de hijos por estado civil de la madre
Nacimientos200922 <- read_excel(
  "Nacimientos2009.xls", 
   sheet = "Estado civil y depto. resi.", 
   range = "A2:F3", col_types = c("text", 
   "numeric", "numeric", "numeric", 
   "numeric", "numeric")
)
Nacimientos20102 <- read_excel(
  "Nacimientos2010.xls", 
  sheet = "Estado civil y depto. resid.", 
  range = "A2:F3", col_types = c("text", 
                                 "numeric", "numeric", "numeric", 
                                 "numeric", "numeric")
)
Nacimientos20112 <- read_excel(
  "Nacimientos2011.xls", 
  sheet = "Estado civil y depto. resi.", 
  range = "A2:F3", col_types = c("text", 
                                 "numeric", "numeric", "numeric", 
                                 "numeric", "numeric")
)
Nacimientos20122 <- read_excel(
  "Nacimientos2012.xls", 
  sheet = "Estado civil y depto. resi.", 
  range = "A2:F3", col_types = c("text", 
                                 "numeric", "numeric", "numeric", 
                                 "numeric", "numeric")
)
Nacimientos20132 <- read_excel(
  "Nacimientos2013.xls", 
  sheet = "Estado civil y depto. resi.", 
  range = "A2:F3", col_types = c("text", 
                                 "numeric", "numeric", "numeric", 
                                 "numeric", "numeric")
)
Nacimientos20142 <- read_excel(
  "Nacimientos2014.xls", 
  sheet = "Estado civil y depto. resi.", 
  range = "A2:F3", col_types = c("text", 
                                 "numeric", "numeric", "numeric", 
                                 "numeric", "numeric")
)
Nacimientos20152 <- read_excel(
  "Nacimientos2015.xls", 
  sheet = "Estado civil y depto. resi.", 
  range = "A2:F3", col_types = c("text", 
                                 "numeric", "numeric", "numeric", 
                                 "numeric", "numeric")
)
Nacimientos20162 <- read_excel(
  "Nacimientos2016.xlsx", 
  sheet = "Estado civil y depto. resi.", 
  range = "A2:F3", col_types = c("text", 
                                 "numeric", "numeric", "numeric", 
                                 "numeric", "numeric")
)
Nacimientos20172 <- read_excel(
  "Nacimientos2017.xlsx", 
  sheet = "Estado civil y depto. resi.", 
  range = "A2:F3", col_types = c("text", 
                                 "numeric", "numeric", "numeric", 
                                 "numeric", "numeric")
)
Nacimientos20182 <- read_excel(
  "Nacimientos2018.xlsx", 
  sheet = "Estado civil y depto. resi.", 
  range = "A2:F3", col_types = c("text", 
                                 "numeric", "numeric", "numeric", 
                                 "numeric", "numeric")
)
Nacimientos20192 <- read_excel(
  "Nacimientos2019.xlsx", 
  sheet = "Estado civil y depto. resi.", 
  range = "A2:F3", col_types = c("text", 
                                 "numeric", "numeric", "numeric", 
                                 "numeric", "numeric")
)
years <- c('2009','2010','2011','2012','2013','2014','2015','2016','2017','2018','2019')
casado2009<-Nacimientos2009$Casada
soltero2009<-Nacimientos2009$Soltera
casado2010<-Nacimientos2010$Casada
soltero2010<-Nacimientos2010$Soltera
casado2011<-Nacimientos2011$Casada
soltero2011<-Nacimientos2011$Soltera
casado2012<-Nacimientos2012$Casada
soltero2012<-Nacimientos2012$Soltera
casado2013<-Nacimientos2013$Casada
soltero2013<-Nacimientos2013$Soltera
casado2014<-Nacimientos2014$Casada
soltero2014<-Nacimientos2014$Soltera
casado2015<-Nacimientos2015$Casada
soltero2015<-Nacimientos2015$Soltera
casado2016<-Nacimientos2016$Casada
soltero2016<-Nacimientos2016$Soltera
casado2017<-Nacimientos2017$Casada
soltero2017<-Nacimientos2017$Soltera
casado2018<-Nacimientos2018$Casada
soltero2018<-Nacimientos2018$Soltera
casado2019<-Nacimientos2019$Casada
soltero2019<-Nacimientos2019$Soltera
HijosCasados <- c(casado2009, casado2010, casado2011, casado2012, casado2013, casado2014, casado2015, casado2016, casado2017, casado2018, casado2019)
HijosSoltero <- c(soltero2009, soltero2010, soltero2011, soltero2012, soltero2013, soltero2014, soltero2015, soltero2016, soltero2017, soltero2018, soltero2019)
Total_hijos <- data.frame(Year = years,
               Casada = HijosCasados,
              Soltera = HijosSoltero)
{ggplot() + 
    geom_line(aes(y = Casada,x = Year,group = 1),
              data = Total_hijos) + 
    geom_vline(xintercept = 12,linetype="dashed",col="red")}
{ggplot() + 
    geom_line(aes(y = Soltera,x = Year,group = 1),
              data = Total_hijos) +
    geom_vline(xintercept = 12,linetype="dashed",col="black")}

{plot(Total_hijos$Casada,type="o",col="darkgreen",ylim=c(0,250000),main="Total de hijos respecto el estado civil de la madre 2009-2019",ylab="No. de hijos",xlab="Años",xaxt="n")
  lines(Total_hijos$Soltera,type="o",col="purple")
  legend(5,100000,legend=c("Casada","Soltera"),
         col=c("darkgreen","purple"),lty=1.2,cex=0.8)
  axis(1,1:11,years)}
### Edad de la madre
Nacimientos2009 <- read_excel("Nacimientos2009.xls", 
                              sheet = "Edad y grupos ocupacionales", 
                              range = "A2:L3", col_types = c("text", 
                                                             "numeric", "numeric", "numeric", 
                                                             "numeric", "numeric", "numeric", 
                                                             "numeric", "numeric", "numeric", 
                                                             "numeric", "numeric"))
Nacimientos2010 <- read_excel("Nacimientos2010.xls", 
                              sheet = "Edad y grupos ocupacionales", 
                              range = "A2:L3", col_types = c("text", 
                                                             "numeric", "numeric", "numeric", 
                                                             "numeric", "numeric", "numeric", 
                                                             "numeric", "numeric", "numeric", 
                                                             "numeric", "numeric"))
Nacimientos2011 <- read_excel("Nacimientos2011.xls", 
                              sheet = "Edad y grupos ocupacionales", 
                              range = "A2:L3", col_types = c("text", 
                                                             "numeric", "numeric", "numeric", 
                                                             "numeric", "numeric", "numeric", 
                                                             "numeric", "numeric", "numeric", 
                                                             "numeric", "numeric"))
Nacimientos2012 <- read_excel("Nacimientos2012.xls", 
                              sheet = "edad y grupos ocupacionales", 
                              range = "A2:F3", col_types = c("text", 
                                                             "numeric", "numeric", "numeric", 
                                                             "numeric", "numeric"))
Nacimientos2013 <- read_excel("Nacimientos2013.xls", 
                              sheet = "edad y grupos ocupacionales", 
                              range = "A2:L3", col_types = c("text", 
                                                             "numeric", "numeric", "numeric", 
                                                             "numeric", "numeric", "numeric", 
                                                             "numeric", "numeric", "numeric", 
                                                             "numeric", "numeric"))
Nacimientos2014 <- read_excel("Nacimientos2014.xls", 
                              sheet = "edad y grupos ocupacionales", 
                              range = "A2:L3", col_types = c("text", 
                                                             "numeric", "numeric", "numeric", 
                                                             "numeric", "numeric", "numeric", 
                                                             "numeric", "numeric", "numeric", 
                                                             "numeric", "numeric"))
Nacimientos2015 <- read_excel("Nacimientos2015.xls", 
                              sheet = "edad y grupos ocupacionales", 
                              range = "A2:L3", col_types = c("text", 
                                                             "numeric", "numeric", "numeric", 
                                                             "numeric", "numeric", "numeric", 
                                                             "numeric", "numeric", "numeric", 
                                                             "numeric", "numeric"))
Nacimientos2016 <- read_excel("Nacimientos2016.xlsx", 
                              sheet = "edad y grupos ocupacionales", 
                              range = "A2:L3", col_types = c("text", 
                                                             "numeric", "numeric", "numeric", 
                                                             "numeric", "numeric", "numeric", 
                                                             "numeric", "numeric", "numeric", 
                                                             "numeric", "numeric"))
Nacimientos2017 <- read_excel("Nacimientos2017.xlsx", 
                              sheet = "edad y grupos ocupacionales", 
                              range = "A2:L3", col_types = c("text", 
                                                             "numeric", "numeric", "numeric", 
                                                             "numeric", "numeric", "numeric", 
                                                             "numeric", "numeric", "numeric", 
                                                             "numeric", "numeric"))
Nacimientos2018 <- read_excel("Nacimientos2018.xlsx", 
                              sheet = "edad y grupos ocupacionales", 
                              range = "A2:L3", col_types = c("text", 
                                                             "numeric", "numeric", "numeric", 
                                                             "numeric", "numeric", "numeric", 
                                                             "numeric", "numeric", "numeric", 
                                                             "numeric", "numeric"))
Nacimientos2019 <- read_excel("Nacimientos2019.xlsx", 
                              sheet = "edad y grupos ocupacionales", 
                              range = "A2:L3", col_types = c("text", 
                                                             "numeric", "numeric", "numeric", 
                                                             "numeric", "numeric", "numeric", 
                                                             "numeric", "numeric", "numeric", 
                                                             "numeric", "numeric"))
joven152009 <- Nacimientos2009$`15 - 19`
joven202009 <- Nacimientos2009$`20 - 24`
joven252009 <- Nacimientos2009$`25 - 29`
joven152010 <- Nacimientos2010$`15 - 19`
joven202010 <- Nacimientos2010$`20 - 24`
joven252010 <- Nacimientos2010$`25 - 29`
joven152011 <- Nacimientos2011$`15 - 19`
joven202011 <- Nacimientos2011$`20 - 24`
joven252011 <- Nacimientos2011$`25 - 29`
joven152012 <- Nacimientos2012$`15 - 19`
joven202012 <- Nacimientos2012$`20 - 24`
joven252012 <- Nacimientos2012$`25 - 29`
joven152013 <- Nacimientos2013$`15 - 19`
joven202013 <- Nacimientos2013$`20 - 24`
joven252013 <- Nacimientos2013$`25 - 29`
joven152014 <- Nacimientos2014$`15 - 19`
joven202014 <- Nacimientos2014$`20 - 24`
joven252014 <- Nacimientos2014$`25 - 29`
joven152015 <- Nacimientos2015$`15 - 19`
joven202015 <- Nacimientos2015$`20 - 24`
joven252015 <- Nacimientos2015$`25 - 29`
joven152016 <- Nacimientos2016$`15 - 19`
joven202016 <- Nacimientos2016$`20 - 24`
joven252016 <- Nacimientos2016$`25 - 29`
joven152017 <- Nacimientos2017$`15 - 19`
joven202017 <- Nacimientos2017$`20 - 24`
joven252017 <- Nacimientos2017$`25 - 29`
joven152018 <- Nacimientos2018$`15 - 19`
joven202018 <- Nacimientos2018$`20 - 24`
joven252018 <- Nacimientos2018$`25 - 29`
joven152019 <- Nacimientos2019$`15 - 19`
joven202019 <- Nacimientos2019$`20 - 24`
joven252019 <- Nacimientos2019$`25 - 29`
rango_15_19 <- c(joven152009, joven152010, joven152011, joven152012, joven152013, joven152014, joven152015, joven152016, joven152017, joven152018, joven152019)
rango_20_24 <- c(joven202009, joven202010, joven202011, joven202012, joven202013, joven202014, joven202015, joven202016, joven202017, joven202018, joven202019)
rango_24_30 <- c(joven252009, joven252010, joven252011, joven252012, joven252013, joven252014, joven252015, joven252016, joven252017, joven252018, joven252019)
Edad_madre <- data.frame(Year = years,
                                "15-19" = rango_15_19,
                                "20-24" = rango_20_24,
                                "25-29" = rango_24_30)

{plot(Edad_madre$X15.19,type="o",col="blue",ylim=c(0,125000),main="No. total de nacimientos con respecto a la edad de la madre",ylab="No. de nacimientos",xlab="Años",xaxt="n")
  lines(Edad_madre$X20.24,type="o",col="purple")
  lines(Edad_madre$X25.29,type="o",col="red")
  legend(8,25000,legend=c("15-19","20-24", "25-29"),
         col=c("Blue","purple", "red"),lty=1.2,cex=0.8)
  axis(1,1:11,years)}

###Clusters 3

Proyecto <- read_excel("Proyecto.xls", range = "B1:Z11")
datosC <- as.matrix(scale(Proyecto))
fviz_nbclust(datosC, kmeans, method = "silhouette")
km<-kmeans(datosC, 2, iter.max = 100)
fviz_cluster(km, data = datosC, frame.type = "convex", geom = "point")
print(km)
clusterM <- aggregate(Proyecto, by=list(cluster=km$cluster), mean)
table(clusterM$SalePrice)
print(clusterM)
km$centers

#cluster 1
Matrimonio2009 <- read_excel("Matrimonio2009.xls", 
                             sheet = "Grupos de edad novio y novia", 
                             skip = 1)
Matrimonio2010 <- read_excel("Matrimonio2010.xls", 
                             sheet = "Grupos de edad novio y novia", 
                             skip = 1)
Matrimonio2011 <- read_excel("Matrimonio2011.xls", 
                             sheet = "Grupos de edad novio y novia", 
                             skip = 1)
Matrimonio2012 <- read_excel("Matrimonio2012.xls", 
                             sheet = "Grupos de edad novio y novia", 
                             skip = 1)
Matrimonio2013 <- read_excel("Matrimonio2013.xls", 
                             sheet = "Grupos de edad novio y novia", 
                             skip = 1)
Matrimonio2014 <- read_excel("Matrimonio2014.xls", 
                             sheet = "Grupos de edad novio y novia", 
                             skip = 1)
Matrimonio2015 <- read_excel("Matrimonio2015.xls", 
                             sheet = "Grupos de edad novio y novia", 
                             skip = 1)
hola <- read_excel("Matrimonio2016.xlsx", 
                             sheet = "Grupos de edad novio y novia", 
                             skip = 1)
hola2 <- read_excel("Matrimonio2017.xlsx", 
                             sheet = "Grupos de edad novio y novia", 
                             skip = 1)
Matrimonio2018 <- read_excel("Matrimonio2018.xlsx", 
                             sheet = "Grupos de edad novio y novia", 
                             skip = 1)
Matrimonio2019 <- read_excel("Matrimonio2019.xlsx", 
                             sheet = "Grupos de edad novio y novia", 
                             skip = 1)
combineData <- rbind(Matrimonio2009, Matrimonio2010)
combineData2 <- rbind(Matrimonio2011, Matrimonio2012)
combineData3 <- rbind(Matrimonio2013, Matrimonio2014)
combineData4 <- rbind(Matrimonio2014, Matrimonio2015)
combineData5 <- rbind(Matrimonio2014, hola2)
combineData6 <- rbind(Matrimonio2018, Matrimonio2019)
combinedata22 <- rbind(combineData,combineData2)
combinedata23 <- rbind(combineData3,combineData)
comvinesajfl <- rbind(combinedata22, combinedata23)
datos<-na.omit(datos)
comvinesajfl$Total
datosC <- select(comvinesajfl ,Total,`Edad del novio`,...4,...5,...6,...7,...8,...9,...10,...11,...12,...13,
                       ...14,...15)
datosC <- (scale(datos))
fviz_nbclust(datosC, kmeans, method = "silhouette")
km<-kmeans(datosC, 2, iter.max = 100)
fviz_cluster(km, data = datosC, frame.type = "convex", geom = "point")
print(km)
clusterM <- aggregate(datosC, by=list(cluster=km$cluster), mean)
table(clusterM$SalePrice)
print(clusterM)
km$centers

