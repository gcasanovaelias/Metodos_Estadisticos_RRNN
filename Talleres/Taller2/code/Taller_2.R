####Taller 2####
#Fecha:25-Marzo-2022

rm(list=ls())

#generacion de valores aleatorios
set.seed(1234)
val <- rnorm(mean=2, n=100, sd=1) #distribucion normal
val
mean(val)
sd(val)

##Vectores y condiciones
c(31, 10, 45, 46) > 31
c(31, 10, 45, 46) >= 31
c(31, 10, 45, 46) != 31
c(31, 10, 45, 46) == 31

c("maiz","cebada", "trigo") >= "maiz"

##Secuencias con intervalos
sec <- seq(1,19,3) #vector incremental
sec
sec <- seq(19,1,-3) #vector decremental
sec

##Pegar elementos de vectores  
paste("los años son", 12)
paste("los años son", sec)
paste0("los años son", sec, "décadas")

#Secuencias alternadas
rep(c(1:3),  each = 4)
rep(c(1:3),  4)
rep(c("maiz","trigo","cebada" ),  4)
rep(c("maiz","trigo","cebada" ),  each=4)

##carga de datos a la consola (desde portapapeles)
data <- read.table("clipboard", header=TRUE)
data <- read.table(pipe("pbpaste"), header=TRUE)
str(data)

#Promedio de datos
mean(as.numeric(data$MED), na.rm = T)

#Ordenar una columna
sort(data$MAX, decreasing = FALSE)

#Encontrar elementos en base a condicion
posis <- which(is.na(data$MIN))
posis
faltan <- length(which(is.na(data$MIN)))
faltan
faltan <- length(posis)

##funciones de importancia
#c(), paste(), paste0(), read.table(), which(), length()


#Creacion de matrices y operatoria de matrices
m1 <- matrix(1:12, nrow=4,ncol=3, byrow = T,
             dimnames = list(paste("p",1:4,sep=""),
                             c("dia1","dia2","dia3")))
m2 <- matrix(9:1, 3, 3,dimnames = list(paste("b",1:3,sep=""),c("dia1","dia2","dia3")))
m1
m2

m1*2
m1-3
m2/4

m1%*%m2 #Multiplicacion matricial
#Regla: ncol_m1=nrow_m2 y result: nrow_m1xncol_m2

#Generacion de tabla matricial (coercion)
especie <- c("rosa", "clavel", "margarita", "lirio", "paciflora", "tulipan")
coef_ex <- c(2,5,13,8,16,6)
long_onda <- c(3,6,2,7,8,2)
matrix(c(especie,coef_ex,long_onda),6,3,byrow = F)

#Generacion de tabla matricial (sin coercion)
df <- data.frame(especie,coef_ex,long_onda)
str(df)

#Subseteo de elementos dentro de una matriz
m1

m1[c(1,4),2:3]

m1[-c(1,3),-2]
m1[-c(1,3), 2]

data[ ,1]
data$FECHA

#Convertir factor a fecha

data$FECHA <- as.Date(data$FECHA, format = "%d-%m-%Y")
str(data$FECHA)
data$FECHA

#Establecer directorio de trabajo
setwd("C:/Users/davmo/Desktop/Cursos_UC/Metodos/datos")

#Cargar los datos
data <- read.csv("datos_torres.csv", header = TRUE,
                 stringsAsFactors = FALSE) 


#Funcion aggregate
ws_mes <- aggregate(data[,c(7,8)], list(data$Mes),
                    mean, na.rm=TRUE)
ws_mes

ws_mes_dia <- aggregate(data$wnd_spd, 
                        list(data$Dia, data$Mes),
                        sd, na.rm=TRUE)
ws_mes_dia

#Funcion apply
colnames(data) #ayuda para identificar posicion

apply(data[ ,c(7,8)], 2, min, na.rm = TRUE) #1 fila, 2 columna
apply(data[ ,c(7,8)], 2, max, na.rm = TRUE)
