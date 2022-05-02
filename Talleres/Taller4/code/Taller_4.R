####Taller 4 metodos####
#fecha: 08 abril 2022
#autor: D. Morales

rm(list=ls())

if (!require("caret")) install.packages("caret")
if (!require("outliers")) install.packages("outliers")
if (!require("AUC")) install.packages("AUC")

##Carga y exploración de los datos
data <- read.csv("C:/Users/davmo/Desktop/Cursos_UC/Metodos/Talleres/Taller4/datos/Arboles.csv",header=T)
str(data) #estructura de los datos

summary(data) #Resumen de los datos
attach(data) #Reserva los nombres de columna como nombre
#de objeto

##Aplicación de un modelo lineal simple
lm1 <- lm(h ~ edad, data = data)
summary(lm1) #Resumen del modelo
plot(edad, h)
abline(lm1) #Linea al de tendencia

#Aplicación de un modelo lineal múltiple
data$log.d <- log(d)

lm2<-lm(h ~ edad + log.d, data=data)
summary(lm2)

anova(lm1, lm2) # Escogemos el que presenta menor suma de residuos cuadrados (menor error) si es que se presenta una diferencia significativa

####Y si aplicamos un modelo Generalizado con distribución exponencial####
glm1 <- glm(h ~ edad, data = data, 
            family = Gamma(link = "inverse"))
summary(glm1)

glm2 <- glm(h ~ edad + d, data = data, 
            family = Gamma(link = "inverse"))
summary(glm2)

glm3 <- glm(h ~ edad + log.d, data = data, 
            family = Gamma(link = "inverse"))
summary(glm3)

anova(glm1, glm2, glm3)

##Que es mejor... podemos ver en funcion de las predicciones.
tab <- data.frame(obs=data$h, glm=glm2$fitted.values,
                  lm=lm2$fitted.values, glm3=glm3$fitted.values)
mae.lm <- mean(abs(tab$obs - tab$lm))
mae.glm <- mean(abs(tab$obs - tab$glm3))
mae.lm
mae.glm
#En base al MAE el lm es mejor modelo 
#pero nos puede dar alturas negativas :s


####Modelo de Poisson####
cancer <- read.csv("http://stat.ufl.edu/~aa/glm/data/Cancer.dat", sep="")
str(cancer)
summary(cancer)

glm3 <- glm(count ~ factor(histology) + 
              factor(stage) + 
              factor(time), 
            family = poisson(link = log), 
            data = cancer)

# ¿Por qué como factor? Para que cada una de las clases que componen el factor (predictando) tengan una incidencia en los modelos

summary(glm3)
(glm3$null.deviance-glm3$deviance)/glm3$null.deviance#Poder predictivo del modelo
# El modelo explica un 80.9% de la variabilidad de los datos. Esto es comparable con el r.squared de los modelos lineales.

#Como lo hubieramos hecho de forma lineal (es mejor?)
lm3 <-  lm(count ~ histology + stage + time, data = cancer)
summary(lm3)

#Evaluemos los desempeños en base a las predicciones.
tab1 <- data.frame(obs=cancer$count, glm=glm3$fitted.values,
                   lm=lm3$fitted.values)
mae.lm <- mean(abs(tab1$obs - tab1$lm))
mae.glm <- mean(abs(tab1$obs - tab1$glm))
mae.lm
mae.glm
#En base al MAE el glm es mejor modelo


####Modelo binomial (logístico)####

data <- read.csv("Talleres/Taller4/data/data.csv", stringsAsFactors = FALSE)
data <- data[,-ncol(data)] #fuera peso
data <- data[,-ncol(data)] #fuera temperatura

data[which(data$Sexo=="Macho"),2] <- "Male"
data[which(data$Sexo=="Hembra"),2] <- "Female"

glimpse(data)
str(data)

data$Localidad <- as.factor(data$Localidad)
data$Sexo <- as.factor(data$Sexo)

#DetecciÃ³n de datos atipicos con test de grubb
for(i in 3:ncol(data)){
  for(j in 1:nrow(data)){
    ind <- j
    if(j==1){  
      dat <- data[,i] #i
    }else{
      dat <- dat
    }
    pval <- grubbs.test(dat)$p.value
    if(pval<0.05){
      #break
      ext <- c(min(dat, na.rm = TRUE), max(dat, na.rm = TRUE))
      cmp <- which(abs(ext - median(dat, na.rm = TRUE)) == max(abs(ext - median(dat, na.rm = TRUE)), na.rm = TRUE))
      posi <- ext[cmp]
      dat[which(dat==posi)] <- NA
      if(j==1){df1 <- posi}else{df1 <- c(df1,posi)}
    }else{
      next
    }
  }
  if(exists("df1")){dj <- paste(df1, collapse = "_");rm("df1")}else{dj <- "No out"}
  
  if(i==3){
    dh <- dj
  }else{
    dh <- data.frame(dh, dj, stringsAsFactors = FALSE)
  }
}
colnames(dh) <- colnames(data)[-c(1,2)]

# El glm binomial es muy sensible a los valores atípicos. Afectan mucho el desplazamiento de la curva.Sí o sí hay que sacarlos.
data2 <- data
for(i in 1:ncol(dh)){
  if(dh[1,i]=="No out"){
    next
  }else{
    val <- as.numeric(unlist(strsplit(dh[1,i], "_")))
    posi <- which(data2[,i+2] %in% val)
    if(length(posi)==0){next}else{data2 <- data2[-posi,]}
  }
}

# El modelo
fit1 <- glm(
  as.formula(
    paste(
      paste(colnames(data2)[2],"~"),
      paste(colnames(data2)[-c(1,2)],
            collapse = " + ")
      )
    ),
  data=data2,
  family=binomial(link = "logit")
  ) #probit

# STEP: Sirve para ver el peso de cada uno de los predictando, evaluando según AIC. "both" significa 
stepwise <- step(fit1, direction="both")
summary(stepwise)

prob <- predict(stepwise, newdata = data2[, -c(1,2)], type = "response")
prob

ss<-data.frame(vals=(data2[,2]), probs=prob)
ss$Est <- ifelse(ss[,2]<0.5,"Female","Male")

# Verificación
cm5.1 <- caret::confusionMatrix(as.factor(ss$Est), data2$Sexo)
cm5.1 #Matriz de confusión, se puede extraer mucha infromación de ella

# Accuracy: % de predicciones correctas independiente de la clase

# Curva ROC
# Evaluación de la predicción del modelo de clasificación
# Relación entre la sensitividad (poder predictivo asociado a una clase en particular (macho o hembra) y 1-especificidad)
pb.bt <- 1-predict(stepwise,newdata=data2[,-c(1,2)],type="response") #Queremos hembra

pred.bin <- data.frame(data2[,2], pb.bt)
colnames(pred.bin) <- c("target","score")

labels.bt <- as.factor(ifelse(pred.bin$target=="Female", 1, 0))
predictions.bt <- pred.bin$score
auc.bt <- AUC::auc(AUC::roc(predictions.bt, labels.bt), min = 0, max = 1)        

plot(AUC::roc(predictions.bt, labels.bt), min=0, max=1, type="l", 
     main=paste0("GLM Logit (", round(auc.bt,3), ")"))
# Nos interesa que la curva esté más pegada a la zona superior izquierda
# La curva final indica el poder predictivo total del modelo
# en base a como cambian las clasificaciones (empeoran/mejoran)
# Segun cambien los umbrales de corte
# Desempeño a partir de la curva ROC
# 0.848 refleja el área bajo la curva antes de llegar a la línea de identidad. Es distinto a la precision. Se incorpora la clasificación erronea de algunos casos.
