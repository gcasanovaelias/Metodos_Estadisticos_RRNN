####Taller 5 metodos 2022 ####
#fecha: 29 abril 2022
#autor: D. Morales

# Los if, ifelse, for, y otros ciclos se denominan como estructuras de control. Otras estructuras de control son:
# stop(): Lo que aparece al correr codigos, utilizados por programadores al construir codigos.


if (!require("glmnet")){install.packages("glmnet")}else{library(glmnet)}
if (!require("foreign")){install.packages("foreign")}else{library(foreign)}
if (!require("haven")){install.packages("haven")}else{library(haven)}
if (!require("fastDummies")){install.packages("fastDummies")}else{library(fastDummies)}

#Regularizacion de modelo lineal para estimar ingresos del trabajo

#limpiamos entorno
rm(list=ls())

#ruta y directorio de nuestros datos
setwd("C:/Users/davmo/Desktop/Cursos_UC/Metodos/Talleres/Taller5/datos")

#Cargar los datos a la consola
data <- read.csv("datos_casen2017.csv", header=TRUE)
#data <- read.csv("datos_casen2017.csv", header=TRUE, fileEncoding = "Latin1")

#Analisis muy sencillo de algunas variables
table(data$region_Región.de.Antofagasta)
table(data$zona_Rural)
mean(data$esc)
hist(data$esc)
sd(data$esc)

#definimos los set para el modelo, x e y
#En rigor los datos se escalan, pero en este caso no los hacemos ya que casi todas las variables predictoras son binarias (0,1)
x <- model.matrix(ytrabajocor ~ ., data = data)[, -1]
y <- data$ytrabajocor

#MODELO RIDGE (L2)
modelos_ridge <- glmnet(x = x, y = y, alpha = 0)
coef(modelos_ridge)
modelos_ridge$lambda

#Ploteamos la magnitud de los coeficientes segun aumentamos el lambda
plot(modelos_ridge, xvar = "lambda", label = TRUE)

set.seed(123)
#Calculamos los errores estimando un modelos con 10 k-folds
cv_error_ridge <- cv.glmnet(x = x, y = y, alpha = 0, nfolds = 10,
                            type.measure = "mse")
#Ploteamos errores
plot(cv_error_ridge)

#* Se calcula un 10 fold CV para cada uno de los lambdas, cada uno de los CV corresponde a un punto rojo y los intervalos de confianza. Si no se le agrega un lambda a la función, esta calcula por defecto 100.

#Obtenemos el lambda quye minimiza los errores
lambda_min <- cv_error_ridge$lambda.min

#Obtenemos el lambda con el cual el error no se aleja por mas de una unidad de desviacion
#estandar del error minimo posible
lambda_opt <- (cv_error_ridge$lambda.1se)


modelo_final_ridge <- glmnet(x = x, y = y, alpha = 0, lambda = lambda_opt)
coef(modelo_final_ridge)


#MODELO LASSO (L1)

modelos_lasso <- glmnet(x = x, y = y, alpha = 1)
coef(modelos_lasso)
plot(modelos_lasso, xvar = "lambda", label = TRUE)

set.seed(123)
cv_error_lasso <- cv.glmnet(x = x, y = y, alpha = 1, nfolds = 10)
plot(cv_error_lasso)

lasso_lambda_min <- log(cv_error_lasso$lambda.min)
lasso_lambda_opt <- cv_error_lasso$lambda.1se

modelo_final_lasso <- glmnet(x = x, y = y, alpha = 1, 
                             lambda = lasso_lambda_opt)
coef(modelo_final_lasso)

#MODELO elasticnet
a <- Sys.time()#Hora de incicio
for(i in 0:50){ #Cuando i=0 estamos en Ridge (L2) y cuando i=50 en Lasso (L1)
  #modelos_elst <- glmnet(x = x, y = y, alpha = i/100)
  #plot(modelos_lasso, xvar = "lambda", label = TRUE)
  
  set.seed(123)
  cv_error_elst <- cv.glmnet(x = x, y = y, alpha = i/50, nfolds = 10)#alpha oscila desde 0 (0/50) hasta 1 (50/50)
  #plot(cv_error_lasso)
  
  #lasso_lambda_min <- log(cv_error_lasso$lambda.min)
  elst_lambda_opt <- cv_error_elst$lambda.1se
  
  modelo_final_elst <- glmnet(x = x, y = y, alpha = i/50, lambda = elst_lambda_opt)
  #coef(modelo_final_lasso)
  val <- modelo_final_elst$dev.ratio #Poder predictivo
  if(i==0){df <- val}else{df <- c(df,val)}
}
b <- Sys.time()#Hora de término
b-a #Tiempo transcurrido

alfa_fin <- c(0:50)[which(df==max(df))]/50 #Valor de alfa que maximiza poder predictivo

#Modelo final
modelo_final_elst <- glmnet(x = x, y = y, alpha = alfa_fin)
coef(modelo_final_elst)
plot(modelo_final_elst, xvar = "lambda", label = TRUE)

set.seed(123)
cv_error_elst <- cv.glmnet(x = x, y = y, alpha = alfa_fin, nfolds = 10)
plot(cv_error_elst)

elst_lambda_min <- log(cv_error_elst$lambda.min)
elst_lambda_opt <- cv_error_elst$lambda.1se

modelo_final_elst <- glmnet(x = x, y = y, alpha = alfa_fin, 
                             lambda = elst_lambda_opt)
coef(modelo_final_elst)

##Un ejemplo especifico usando sintaxis del mundo tidyverse
####Parte 1: Carga de Librerias####
##Datos
if (!require("faraway")){install.packages("faraway")}else{library(faraway)}

##Gráficos y tratamiento de datos
if (!require("tidyverse")){install.packages("tidyverse")}else{library(tidyverse)}
if (!require("scales")){install.packages("scales")}else{library(scales)}
if (!require("corrr")){install.packages("corrr")}else{library(corrr)}

##Modelado
if (!require("glmnet")){install.packages("glmnet")}else{library(glmnet)}

##Datos
data("meatspec", package = "faraway")
datos <- meatspec
head(datos,3)

####Parte 2: Analisis exploratorio####
##Correlación entre columnas numéricas
df_correlaciones <- datos %>%
  correlate(method = "pearson") %>%
  stretch(remove.dups = TRUE)

df_correlaciones %>% mutate(r_abs = abs(r)) %>% arrange(desc(r_abs)) %>% head(5)

####Parte 3: Regresión lineal simple
##División de los datos en train y test
set.seed(1235)
id_train <- sample(1:nrow(datos), size = 0.7*nrow(datos), replace = FALSE)

datos_train <- datos[id_train, ]
datos_test  <- datos[-id_train, ]

##Regresion lineal simple con MCO
modelo <- lm(fat ~ ., data = datos_train)
summary(modelo)

##Predicciones en datos testeo
predicciones_test <- predict(modelo, newdata = datos_test)

##MSE de testeo
test_mse_ols <- mean((predicciones_test - datos_test$fat)^2)
paste("Error (mse) de test:", test_mse_ols)
paste("Error (rmse) de test:", sqrt(test_mse_ols))

####Parte 4: Sleleccion por Stepwise
##Modelo Stepwise
modelo <- step(
  object    = lm(formula = fat ~ ., data = datos_train),
  direction = "backward",
  scope     = list(upper = ~., lower = ~1),
  trace     = FALSE
)

summary(modelo)

# Predicciones en datos de testeo
predicciones_test <- predict(modelo, newdata = datos_test)

# MSE en testeo
test_mse_step <- mean((predicciones_test - datos_test$fat)^2)
paste("Error (mse) de test:", test_mse_step)
paste("Error (rmse) de test:", sqrt(test_mse_step))

####Parte 5: Regularizaciones
#Matrices de entrenamiento y testeo
x_train <- model.matrix(fat~., data = datos_train)[, -1]
y_train <- datos_train$fat

x_test <- model.matrix(fat~., data = datos_test)[, -1]
y_test <- datos_test$fat

##Regularizacion Ridge
#Creación y entrenamiento del modelo
modelo <- glmnet(
  x           = x_train,
  y           = y_train,
  alpha       = 0, #0 corresponde a Ridge
  nlambda     = 100, #Número de lambdas a optener
  standardize = TRUE #Ideal siempre estandarizar los datos
)

#Evolución de los coeficientes en función de lambda
regularizacion <- modelo$beta %>% 
  as.matrix() %>%
  t() %>% 
  as_tibble() %>%
  mutate(lambda = modelo$lambda)

regularizacion <- regularizacion %>%
  pivot_longer(
    cols = !lambda, 
    names_to = "predictor",
    values_to = "coeficientes"
  )

regularizacion %>%
  ggplot(aes(x = lambda, y = coeficientes, color = predictor)) +
  geom_line() +
  scale_x_log10(
    breaks = trans_breaks("log10", function(x) 10^x),
    labels = trans_format("log10", math_format(10^.x))
  ) +
  labs(title = "Coeficientes del modelo en función de la regularización") +
  theme_bw() +
  theme(legend.position = "none")

# Evolución del error en función de lambda
set.seed(123)
cv_error <- cv.glmnet(
  x      = x_train,
  y      = y_train,
  alpha  = 0,
  nfolds = 10,
  type.measure = "mse",
  standardize  = TRUE
)

plot(cv_error)

#Mejor modelo lambda optimo + 1sd
modelo <- glmnet(
  x           = x_train,
  y           = y_train,
  alpha       = 0,
  lambda      = cv_error$lambda.1se,
  standardize = TRUE
)

# Predicciones en datos de testeo
predicciones_test <- predict(modelo, newx = x_test)

# MSE en testeo
test_mse_ridge <- mean((predicciones_test - y_test)^2)
paste("Error (mse) de test:", test_mse_ridge)
paste("Error (rmse) de test:", sqrt(test_mse_ridge))

##Regularizacion Lasso
# Creación y entrenamiento del modelo
modelo <- glmnet(
  x           = x_train,
  y           = y_train,
  alpha       = 1, #1 para Lasso
  nlambda     = 100,
  standardize = TRUE
)

# Evolución de los coeficientes en función de lambda
regularizacion <- modelo$beta %>% 
  as.matrix() %>%
  t() %>% 
  as_tibble() %>%
  mutate(lambda = modelo$lambda)

regularizacion <- regularizacion %>%
  pivot_longer(
    cols = !lambda, 
    names_to = "predictor",
    values_to = "coeficientes"
  )

regularizacion %>%
  ggplot(aes(x = lambda, y = coeficientes, color = predictor)) +
  geom_line() +
  scale_x_log10(
    breaks = trans_breaks("log10", function(x) 10^x),
    labels = trans_format("log10", math_format(10^.x))
  ) +
  labs(title = "Coeficientes del modelo en función de la regularización") +
  theme_bw() +
  theme(legend.position = "none")

# Evolución del error en función de lambda
set.seed(123)
cv_error <- cv.glmnet(
  x      = x_train,
  y      = y_train,
  alpha  = 1,
  nfolds = 10,
  type.measure = "mse",
  standardize  = TRUE
)

plot(cv_error)

#Mejor modelo lambda óptimo + 1sd
modelo <- glmnet(
  x           = x_train,
  y           = y_train,
  alpha       = 1,
  lambda      = cv_error$lambda.1se,
  standardize = TRUE
)

#Predictories considerados y sus coeficientes
df_coeficientes <- coef(modelo) %>%
  as.matrix() %>%
  as_tibble(rownames = "predictor") %>%
  rename(coeficiente = s0)

df_coeficientes %>%
  filter(
    predictor != "(Intercept)",
    coeficiente != 0
  )

# Predicciones en los datos de testeo
predicciones_test <- predict(modelo, newx = x_test)

# MSE en testeo
test_mse_lasso <- mean((predicciones_test - y_test)^2)
paste("Error (mse) de test:", test_mse_lasso)
paste("Error (rmse) de test:", sqrt(test_mse_lasso))


##Tarea volunataria: Pruebe modelos ElasticNet considerando distintos valores de alfa
##(50) espaciados de forma regular entre 0(Ridge) y 1 (Lasso)
##Verifique cuantos predictores contiene el modelo final y pruebelos en los datos
##de testeo reportando el mse