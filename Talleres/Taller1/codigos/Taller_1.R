####Taller 1 metodos 2022 ####
#fecha: 18 marzo 2022
#autor: D. Morales

##Análisis básico
rm(list = c("a", "e")) #ls() #Remueve los objetos del ambiente

a <- 2 #diferencia entre = y <-
b <- 3
d <- a + b
e <- (a + b)/2

##Vectores
d <- c(4, 2.8, 3, 8, 7)
d[c(2:3)]

d #Ejemplo de coercion/Forzamiento

f <- c("casa", 34, "arbol", 45, 28)#Ejemplo de coercion
f

##Operatoria
escalar <- 4
vector <- c(3:45)
vector

vector[12] #devuelve valor en posicion 12
vector[c(8:11)] #Devuelve valor entre las posicion entre 8 a 11
vector[c(7, 38 , 3)]
c(vector[3], vector[7], vector[28])

vector + escalar #suma  
vector - escalar  
vector * escalar

div <- vector / escalar

vector^2
log(vector, 4)
mean(vector)
var(vector)
sd(vector)

vector2 <- c(vector, NA, NULL)

mean(vector2, na.rm = TRUE)
sd(vector2, na.rm = TRUE)
var(vector2, na.rm = TRUE)

##funcion para volumen de cilindro
v.cil <- function(rad=5, h){
  vol <- (pi*rad^2)*h
  print(vol)
}

#Evaluando la función
r1 <- 3
h1 <- 8

v.cil(r1, h1)

