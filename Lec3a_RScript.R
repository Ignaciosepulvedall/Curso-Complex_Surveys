


# limpiamos objetos del espacio de trabajo de R
rm(list=objects())

# establecemos carpeta de trabajo
# !esta ruta deben cambiarla según donde esten guardando el archivo 'PoblacionSimulada.RData'
setwd("C://Users//ASUS//Desktop//MAlvarado_Cursos//S2020_II//USACH//SurveyAnalysis//Lecs//Lec03")

# Tenemos una POBLACIÓN: 'PoblacionSimulada.RData'
load("PoblacionSimulada.RData")

# Cargamos nuestras funciones
load("myFuns.RData")

# N
(N <- dim(C17)[1])

# n
(n <- round(dim(C17)[1]*0.01, 0))

# pi_k
(pi_k <- n/N)

# d_k
(d_k <- 1/pi_k)

# En nuestra población
C17$pi_k <- n/N
C17$d_k <- 1/C17$pi_k

# MAS
set.seed(1123)
muestra <- ma_muestra(C17, n)

# Población
dim(C17)

# Muestra
dim(muestra)

# Población
head(C17, n = 9)

# Muestra
head(muestra, n = 9)


# Cargamos las librerias necesarias: survey
library(grid)
library(Matrix)
library(survival)
library(survey)

# Diseño Muestral MAS sin reemplazo
muestra.dsg <- svydesign(id=~1, weights=~d_k, data=muestra)

# Estimamos el total de la PET y su varianza
svytotal(~pet, muestra.dsg, na.rm = TRUE)

# PET (parámetro)
par_pet


# Estimamos el total por sexo y su varianza
svytotal(~ factor(sexo), muestra.dsg, na.rm = TRUE)

# Totales por sexo (parámetro)
table(C17$sexo)
