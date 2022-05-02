####Taller 5 metodos 2021 ####
#fecha: 22 abril 2021
#autor: D. Morales

rm(list=ls())

if (!require("class")) install.packages("class")
if (!require("car")) install.packages("car")
if (!require("SemiPar")) install.packages("SemiPar")
if (!require("lattice")) install.packages("lattice")

library(class)
library(car)
library(SemiPar)
library(lattice)

###K-nn####
df <- iris
head(iris)

set.seed(123)
ran <- sample(1:nrow(iris), 0.9 * nrow(iris)) 

##Crear funcon para normalizar datos
nor <- function(x) { (x -min(x))/(max(x)-min(x))   }

##Correr la normalizacion en los predictores
iris_norm <- as.data.frame(lapply(iris[,1:4], nor))

iris_train <- iris_norm[ran,] ##Datos entrenamiento
iris_test <- iris_norm[-ran,]  ##DAtos testeo
iris_target_c <- iris$Species[ran]
iris_test_c <- iris$Species[-ran]

# FUNCIÓN knn (frecuencias)
pr <- knn(
  train = iris_train,
  test = iris_test,
  cl=iris_target_c,
  k=10
  )
# kmeans() para distancia

##Crea matriz de confusion
tab <- table(pr,iris_test_c)
tab

#Precision
accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracy(tab)


##Regresión parametricas - no parametricas
data(lidar)

plot(logratio~range,data=lidar)

##Regresion polinomial
cols <- rainbow(5)
range.seq <- seq(min(lidar$range),max(lidar$range),l=200)
plot(logratio~range,data=lidar,cex=.5,pch=19,col="grey",xlim=c(380,725),ylim=c(-0.90,0.05))
for(i in 1:5){
  lines(range.seq,predict(lm(logratio~poly(range,i),data=lidar),data.frame(range=range.seq)),lwd=2,col=cols[i])
}
legend("bottomleft",c("p = 1","p = 2","p = 3","p = 4","p = 5"),col=cols,lty=1:5,lwd=4)

##Regresion LOESS (% de la variable x cercanos)
attach(lidar)
span <- 1/c(1,2,3,4,5)
plot(logratio~range,data=lidar,cex=.5,pch=19,col="grey",xlim=c(380,725),ylim=c(-0.90,0.05))
cols <- rainbow(length(span))
for(i in 1:length(span)){
  lines(loess.smooth(range,logratio,span=span[i], degree=2),col=cols[i],lwd=2.5,lty=i)
}
legend("bottomleft",paste("span = ",round(span,3)),col=cols,lwd=2.5,lty=1:6)


#Regresion Kernel (por número/intervalo de la variable x)
b <- c(10,20,50,120,1000)
plot(logratio~range,data=lidar,cex=.5,pch=19,col="grey",xlim=c(380,725),ylim=c(-0.90,0.05))
cols. <- rainbow(length(b))
for(i in 1:length(b)){
  lines(ksmooth(range,logratio,x.points=range,"normal", bandwidth=b[i]),col=cols[i],lwd=2.5,lty=i)
}
legend("bottomleft",paste("bandwidth = ",b),col=cols,lwd=2.5,lty=1:5)

