

####################################################
# Lec.3 Análisis de Encuestas Complejas
# codigo para simular población en base a Censo 2017
####################################################

# limpiamos objetos del espacio de trabajo de R
rm(list=objects())

# establecemos carpeta de trabajo
setwd("C://Users//ASUS//Desktop//MAlvarado_Cursos//S2020_II//USACH//SurveyAnalysis//Lecs//Lec03//SimPoblacionCenso2017")

# fijamos semilla para hacer proceso replicable
set.seed(1123)

# Estructura de la Poblacion por Región, segun sexo y tramos de edad: guardamos datos en el objeto 'Censo2017'
Censo2017 <- read.table(file = "Censo2017xRegion.txt",
                        header = TRUE, dec = ".", sep = "\t")

# Probabilidad de estar ocupado por Región, segun sexo y tramos de edad: guardamos datos en el objeto 'prob'
prob <- read.table(file = "Censo2017ProbOcuxRegion.txt",
                   header = TRUE, dec = ".", sep = "\t")

# colClasses = c("type_var1", ..., "type_varn")

# tipo de objetos
class(Censo2017)
class(prob)


# vemos los primeros datos de nuestros objetos (default n = 6)
head(Censo2017) # poblacion_region_sexo
head(prob) # prob_region_sexo

# podemos personalizar
head(Censo2017, n = 13) # los primeros n = 13 datos
head(prob, n = -1) # todos, menos la última observación (fila)


# información sobre la función 'head'
help(head)


# nombres de los objetos
names(Censo2017)
names(prob)

# personalizamos
names(Censo2017)[1] # solo el nombre de la primera variable (columna)
names(Censo2017)[-1] # todas menos la última variable (columna)
names(prob)[c(1,3,5)] # para un subconjunto
names(Censo2017)[c(1,33)] # para un subconjunto
names(Censo2017)[-c(1,33)] # para un subconjunto

# dimensiones de nuestros objetos
dim(Censo2017)
dim(prob)

# largo la dimensión de nuestros objetos (una función dentro de otra)
length(dim(prob))

# podemos
dim(prob)
dim(prob)[1]


# si queremos acceder a una variable del objeto 'prob': $
# names(prob)
# head(prob)
prob$tramo
Censo2017$p_1_1

# si queremos acceder a una variable simplemente por su nombre
attach(Censo2017)
p_1_1

attach(prob)

# Simulamos la población
# Variables auxiliares: X (region, sexo, grupo de edad)
# Variables de interes: Y (pet, ocu)

# Simulamos poblacion según region x sexo & grupo de edad (quinquenios): objetos 'df_region_sexo'
for (r in 1:16){
  for (s in 1:2){
    ndf <- paste("df", r, s, sep = "_")
    npi <- paste("p", r, s, sep = "_")
    npoi <- paste("po", r, s, sep = "_")

    # variables auxiliares: X
    assign(ndf, data.frame( id = as.integer(seq(1 : sum(eval(parse(text = npi))))),
                            region = as.integer(rep(r, sum(eval(parse(text = npi))))),
                            sexo = as.integer(rep(s, sum(eval(parse(text = npi))))),
                            gedad = as.integer(rep(seq(0,15), eval(parse(text = npi)))),
                            pet = as.integer(rep(seq(0,1), c(eval(parse(text = npi))[1],
                                                           sum(eval(parse(text = npi))[-1])) )),
                            ocu = as.integer(c(rep(NA, eval(parse(text = npi))[1]),
                                               rbinom(eval(parse(text = npi))[2], 1, as.double(eval(parse(text = npoi))[1])),
                                               rbinom(eval(parse(text = npi))[3], 1, as.double(eval(parse(text = npoi))[2])),
                                               rbinom(eval(parse(text = npi))[4], 1, as.double(eval(parse(text = npoi))[3])),
                                               rbinom(eval(parse(text = npi))[5], 1, as.double(eval(parse(text = npoi))[4])),
                                               rbinom(eval(parse(text = npi))[6], 1, as.double(eval(parse(text = npoi))[5])),
                                               rbinom(eval(parse(text = npi))[7], 1, as.double(eval(parse(text = npoi))[6])),
                                               rbinom(eval(parse(text = npi))[8], 1, as.double(eval(parse(text = npoi))[7])),
                                               rbinom(eval(parse(text = npi))[9], 1, as.double(eval(parse(text = npoi))[8])),
                                               rbinom(eval(parse(text = npi))[10], 1, as.double(eval(parse(text = npoi))[9])),
                                               rbinom(eval(parse(text = npi))[11], 1, as.double(eval(parse(text = npoi))[10])),
                                               rbinom(eval(parse(text = npi))[12], 1, as.double(eval(parse(text = npoi))[11])),
                                               rbinom(eval(parse(text = npi))[13], 1, as.double(eval(parse(text = npoi))[12])),
                                               rbinom(eval(parse(text = npi))[14], 1, as.double(eval(parse(text = npoi))[13])),
                                               rbinom(eval(parse(text = npi))[15], 1, as.double(eval(parse(text = npoi))[14])),
                                               rbinom(eval(parse(text = npi))[16], 1, as.double(eval(parse(text = npoi))[15]))) )   )  )

  }
}


# combinamos los objetos por fila (r: row) los objetos: objeto 'C17'
C17 <- rbind(df_1_1, df_1_2, df_2_1, df_2_2, df_3_1, df_3_2, df_4_1, df_4_2, df_5_1, df_5_2,
             df_6_1, df_6_2, df_7_1, df_7_2, df_8_1, df_8_2, df_9_1, df_9_2, df_10_1, df_10_2,
             df_11_1, df_11_2, df_12_1, df_12_2, df_13_1, df_13_2, df_14_1, df_14_2, df_15_1, df_15_2, df_16_1, df_16_2)


# vemos las primeras filas de nuestra población simulada
head(C17)
head(C17)[c(1,2), ] # primeras 2 filas

dim(C17)
head(C17)[c(1,10), ] # ¿por qué no podemos ver la fila 10 de una base de más de 17MM de observaciones?
head(C17, dim(C17)[1])[c(1,10), ]

head(C17, dim(C17)[1])[c(1,100000), ]

# creamos un vector para las ID
nobs <- seq(1, dim(C17)[1])

# desordenamos el vector con las ID (usando la función sample)
ID <- sample(nobs, length(nobs), replace=FALSE)

# reemplazamos variable id por ID
C17$id <- ID
head(C17)

# cargamos el package 'car' (previamente instalado)
library(car)

# recodificamos edad en decenios (x pet)
C17$gedad <- recode(C17$gedad, "0 = 1; 1:2 = 2; 3:4 = 3; 5:6 = 4;
                    7:8 = 5; 9:10 = 6; 11:12 = 7; else = 8")

# POBLACION FINAL SIMULADA:
C17 <- C17[order(C17$id, decreasing = FALSE),]
head(C17)


########################## PARAMETROS POBLACIONALES: Y ##########################
# Total de Personas en Edad de Trabajar (pet)
par_pet <- sum(C17$pet, na.rm = TRUE)

# Total de Personas Ocupadas (ocu)
par_ocu <- sum(C17$ocu, na.rm = TRUE)

# Tasa de Ocupacion (ocu/pet)
par_to <- par_ocu/par_pet


# TOTALES POBLACIONALES (INFORMACION EXOGENA)
Tx_Region_Sexo <- as.matrix(table(C17$region, C17$sexo))

Tx_Region_Edades <- as.matrix(table(C17$region, C17$gedad))

Tx_Region_Edades_Sexo1 <- as.matrix(table(C17$region[C17$sexo == 1], C17$gedad[C17$sexo == 1]))
Tx_Region_Edades_Sexo2 <- as.matrix(table(C17$region[C17$sexo == 2], C17$gedad[C17$sexo == 2]))

# guardamos los objetos dentro de una objeto '.RData'
save(par_pet, par_ocu, par_to, C17, Tx_Region_Sexo, Tx_Region_Edades, Tx_Region_Edades_Sexo1, Tx_Region_Edades_Sexo2,
     file = "PoblacionSimulada.RData")

# limpiamos objetos del espacio de trabajo de R
rm(list=objects())


############################################################################
############################################################################
############################################################################
