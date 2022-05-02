####Taller 3 metodos 2022 ####
#fecha: 01 abril 2021
#autor: D. Morales-Moraga

install.packages("agricolae") #1 vez
install.packages("caret")
library(agricolae) #Cada vez que inicio R
library(caret)

rm(list=ls())
setwd("C:/Users/davmo/Desktop/Cursos_UC/Metodos/Talleres/Taller3")

datos <- read.csv(paste0(getwd(), "/datos/data_alumnos.csv"),header=T, stringsAsFactors = F)
str(datos) #estructura de los datos

summary(datos) #Resumen de los datos
attach(datos) #Reserva los nombres de columna como nombre de objeto
#detach(datos) #Suelta los nombres de columna usados

pairs(datos[,5:7]) #Grafico 1 a 1
cor(datos[,5:7]) #matriz de correlación

#Analisis de varianza
t.test(Estatura_mt~Sexo, data=datos)
an1 <- aov(Estatura_mt~Sexo, data=datos)
summary(an1)

t.test(Peso_kg~Sexo, data=datos)
an2 <- aov(Peso_kg~Sexo, data=datos)
summary(an2)

an3 <- aov(Peso_kg~Dulce, data=datos)
summary(an3)
pairwise.t.test(datos$Peso_kg, datos$Dulce, pool.sd = T, p.adj="bonf")

hsd <- HSD.test(an3, "Dulce") #Test de Tukey
hsd

####Regresiones####

lm1 <- lm(Estatura_mt ~ Edad, data = datos)
summary(lm1) #Resumen del modelo
plot(Edad, Estatura_mt)
abline(lm1) #Linea al de tendencia

lm2<-lm(Estatura_mt ~ Peso_kg)
summary(lm2)
plot(Peso_kg,Estatura_mt, pch=datos$Sexo)
abline(lm2)

lm3 <- lm(Estatura_mt ~ Edad + Peso_kg)
summary(lm3)
plot(lm3$fitted.values, Estatura_mt)
abline(lm3) 

plot(lm3)
plot(lm2)
plot(lm1)

#Voy a sacar info
datos2 <- datos[-c(1, 4, 22), ]

data <- data.frame(datos$Estatura_mt, Est=lm3$fitted.values)

plot(Estatura_mt,lm3$residuals)
abline(0,0)
mean(lm3$residuals)
var(lm3$residuals)

sd(lm3$residuals) #RSME
mean(abs(lm3$residuals)) #MAE
summary(lm3)$r.squared #R cuadrado

shapiro.test(rnorm(length(lm3$residuals), 
                   mean(lm3$residuals), sd(lm3$residuals)))

shapiro.test(lm3$residuals)#Normalidad de residuos

lm4 <- lm(Estatura_mt ~ Edad + Peso_kg + Edad:Peso_kg)
summary(lm4)
plot(lm4$fitted.values, Estatura_mt)
abline(lm4)

#Sumas de cuadrado extras
anova(lm1)
anova(lm2)
anova(lm3)
anova(lm2, lm3)
anova(lm2, lm1,lm3)

###Diagnosticos de regresion###

###Outliers###
install.packages("car")
library(car)

outlierTest(lm4) # Test Bonferonni para observaciones extremas
qqPlot(lm4, main="QQ Plot")
res <- resid(lm4)
hats <- lm.influence(lm4)$hat
ressq <- res^2
plot(ressq,hats) #Revisar esquema

xx <-  #Numero de parametros
outs<-hats[which(hats>((xx*(2+1))/nrow(datos)))] # 2 (primero) es nº de parametros
outs

###Observaciones influyentes####

cutoff <- 4/((nrow(datos)-length(lm4$coefficients)-2)) # identificar valores mayores a 4/(n-k-1)
plot(lm4, which=4, cook.levels=cutoff)

b<-datos[¿?,] #elimino observacion influente
attach(b)

lm5<-lm(Estatura_mt ~ Edad + Peso_kg + Edad:Peso_kg, data=b)
summary(lm5)


###Normalidad residuos###

hist(lm4$residuals)
qqPlot(lm4)
shapiro.test(lm4$residuals) #Ho: Los residuos distribuyen normal

###Homocedasticidad###

ncvTest(lm4) #Ho: var de error cte; Ha: cambia con el nivel de respuesta
ncvTest(lm2)

###Multicolinealidad###
vif(lm4)
vif(lm4) > 5  #regla 1
sqrt(vif(lm4)) > 2 #regla 2
mean(vif(lm4)) > 1
cor(datos$Edad, datos$Peso_kg)

###No independencia de errores (autocorrelación)###

durbinWatsonTest(lm4, simulate=T) #Ho: Autocorrelacion 0 (errores independientes) 

#Modelos lineales mixtos
#install.packages("nlme")
library(nlme)

lme1 <- lme(Estatura_mt ~ Peso_kg, data = datos, 
            random = ~ 1|factor(Sexo))
summary(lme1)
lme1$coefficients

data$Est_mm <- lme1$fitted

anova(lme1, lm3)
AIC(lme1)
AIC(lm3)

#Factor "dulce" como efecto aleatorio
lme2 <- lme(Estatura_mt ~ Peso_kg, data = datos, 
            random = ~ 1|factor(Dulce))
summary(lme2)
lme2$coefficients

anova(lme1, lme2)
#anova(lm3, lme2)

#Lineal simple
mean(abs(data$datos.Estatura_mt-data$Est))#MAE

#Mixto
mean(abs(data$datos.Estatura_mt-data$Est_mm))#MAE

##Forma "correcta" de hacer un modelo predictivo
lm1.loocv <- train(Estatura_mt ~ Peso_kg, method = "lm", data = datos, 
                   trControl = trainControl(method = "LOOCV"))
summary(lm1.loocv)

lm1.cv <- train(Estatura_mt ~ Peso_kg, method = "lm", data = datos, 
                trControl = trainControl(method = "cv", number = 10, p=0.8))
summary(lm1.cv)

lm1.loocv$results
lm1.cv$results

lm1.loocv$control  
lm1.cv$control

lm1.cv$resample


