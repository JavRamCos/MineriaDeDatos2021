### Renato Estrada 
install.packages("caret")
install.packages("dummies")
library("tidyverse")

library("caret")
library("dummies")

porcentaje <- 0.7
set.seed(123)

data <- read.csv("train.csv")
data[is.na(data)] = "NO"

data$clasificacion <- ifelse(data$SalePrice < 132107, "Economicas", ifelse( data$SalePrice > 232669, "Caras", "Intermadias"))
datos <- cbind(data, dummy(data$clasificacion, verbose = T))

corte <- sample(nrow(datos), nrow(datos) * porcentaje)
train<-datos[corte,]
test<-datos[-corte,]

datosC<-select(train, LotArea, TotalBsmtSF, X1stFlrSF, X2ndFlrSF, WoodDeckSF, OpenPorchSF, EnclosedPorch, X3SsnPorch, ScreenPorch, PoolArea, MiscVal, SalePrice, dataCaras, dataEconomicas, dataIntermadias)
datosT<-select(test, LotArea, TotalBsmtSF, X1stFlrSF, X2ndFlrSF, WoodDeckSF, OpenPorchSF, EnclosedPorch, X3SsnPorch, ScreenPorch, PoolArea, MiscVal, SalePrice, dataCaras, dataEconomicas, dataIntermadias)
head(datosC[,c(1:15)])

###Precio caro
modelo<-glm(dataCaras~., data = datosC[,1:13], family = binomial(), maxit=100)
pred<-predict(modelo, newdata = datosT[,1:12], type="response")
prediccion<-ifelse(pred>=0.5,1,0)
confusionMatrix(as.factor(datosT$dataCaras), as.factor(prediccion))

###Precio intermedio
modelo2<-glm(dataIntermadias~., data = datosC[,c(1:12,15)], family = binomial(), maxit=100)
pred2<-predict(modelo2, newdata = datosT[,1:12], type="response")
prediccion2<-ifelse(pred2>=0.5,1,0)
confusionMatrix(as.factor(datosT$dataCaras), as.factor(prediccion2))

###precio bajo
modelo3<-glm(dataEconomicas ~., data = datosC[,c(1:12,14)], family = binomial(), maxit=100)
pred3<-predict(modelo3, newdata = datosT[,1:12], type="response")
prediccion3<-ifelse(pred3>=0.5,1,0)
confusionMatrix(as.factor(datosT$dataCaras), as.factor(prediccion3))
