
############################################
# Análisis de Encuestas Complejas
# Lec 2: Introducción a R (Parte I)
# R Script
############################################

##### GENERALIDADES: Software R y R-Studio

# directorio de trabajo actual
getwd()
# establecer otro directorio de trabajo
setwd("C://Users//ASUS//Desktop//MAlvaradoGitHub")

# establecer directorio de trabajo y asignarlo al objeto "wd"
wd<-setwd("C://Users//ASUS//Desktop//MAlvarado_Cursos//S2020_II//USACH//SurveyAnalysis//Lecs//Lec02")
# el contenido del objeto "wd"
wd



##### TIPOS DE OBJETOS

# reglas para nombrar objetos
help(reserved)


# Los dos objetos más comunes en R: Numéricos y de caracteres
a <- 2 # numericos
class(a)

word <- "Usach" # caracteres (strings)
class(word)

# creado un objeto, este puede ser utilizado en otros objetos y/o funciones
(b <- log(a) + sqrt(a) * 2)


# Otros tipos de objetos
# VECTORES: numéricos y de caracteres
vec1 <- c(1,2,5,0.5,9,8)
length(vec1)

.vec2 <- c("a", "E", "i", "o", "U")
class(.vec2)

Vec3 <- c(2, "a")
class(Vec3)
Vec3

# puede ser utilizado en otros objetos y/o funciones
(log.vec1 <- log(vec1))
(mean.vec1 <- mean(vec1))

# se puede acceder a los elementos individuales con: [ ]
log.vec1[2]


# otras funciones para crear vectores
rep(0, times=397)
rep("xYz", 49)

seq(from=1, to=500, by=2)
seq(from=0, to=-500, by=-2)

-30:70

rep(seq(1,3,1), times=3)


##### FUNCIONES

# estructura de una función
exp(0)

# FUNCIONES GENERALES (por defecto) de R ....
date()

# información de estas funciones, con la función ? o help()
?log
help(mean)

# FUNCIONES DEFINIDAS POR EL USUARIO
mifunxmedia <- function(x){
  n <- length(x)
  xsum <- sum(x)

  xsum/n}

class(mifunxmedia)

mifunxmedia(vec1)
mean(vec1)

# FUNCIONES DENTRO DE PACKAGES DESARROLLADOS
install.packages('foreign')
library(foreign)


##### MAS TIPOS DE OBJETOS
data <- read.dta(file = "data1_stata12.dta")

data

