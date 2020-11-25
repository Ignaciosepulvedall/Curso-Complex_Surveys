
####################################################
# Lec.3 Análisis de Encuestas Complejas
# codigo Lec.3.
####################################################

# limpiamos objetos del espacio de trabajo de R
rm(list=objects())

# establecemos carpeta de trabajo
setwd("C://Users//ASUS//Desktop//MAlvarado_Cursos//S2020_II//USACH//SurveyAnalysis//Lecs//Lec03//SimPoblacionCenso2017")

# cargamos objeto 'PoblacionSimulada.RData'
load("PoblacionSimulada.RData")

# veamos algunos objetos
par_ocu
Tx_Region_Sexo

# inspecciones nuestra poblacion
summary(C17)

# tipo de objeto variable sexo
class(C17$sexo) # es correcto este tipo?
levels(C17$sexo)

# establecemos el tipo correcto: variable factor
C17$sexo <- as.factor(C17$sexo)
class(C17$sexo)

# vemos el tipo de todas las variables
sapply(C17, class)

C17$region <- as.factor(C17$region)
C17$gedad <- as.factor(C17$gedad)

sapply(C17, class)


# definamos funciones
# Funcion que toma los identificadores para una muestra de tamaño s de una población (S): MAS sin reemplazo
sample_s = function(S,s){
  index <- sample(1:nrow(S), s)
  S[index, ]}

# Funcion que toma una muestra de tamaño s de una población (S): MAS sin reemplazo
ma_muestra <- function(S, s){
  sample_s(S,s)}

# definamos tamaño muestral (n muestra)
n <- round(dim(C17)[1]*0.01, 0)
n

# seleccionemos la muestra usando las funciones definidas
muestra <- ma_muestra(C17, n)

# inspecciones nuestra muestra
dim(muestra)
head(muestra)
summary(muestra)

# Totales: freq absolutas
t_sexo <- table(muestra$sexo)
t_region_sexo <- table(muestra$region, muestra$sexo)
t_region_edades_sexo <- table(muestra$region, muestra$gedad, muestra$sexo)

class(t_region_sexo)
dim(t_region_sexo)

class(t_region_edades_sexo)
dim(t_region_edades_sexo)

# Totales: freq relativas
prop.table(t_sexo)
prop.table(t_sexo)*100

prop.table(t_region_sexo)*100

prop.table(t_region_edades_sexo)*100



# Poblacion vs Muestra: Distribución por sexo
prop.table(table(C17$sexo))*100
prop.table(t_sexo)*100

# Parámetros VS Estimaciones (muestrales)
par_to
sum(muestra$ocu, na.rm = TRUE)/sum(muestra$pet, na.rm = TRUE)

par_pet
sum(muestra$pet, na.rm = TRUE)

par_ocu
sum(muestra$ocu, na.rm = TRUE)


##########
# Muestra Probabilística: MAS (igual probabilidad de selección) sin reemplazo
##########
dim(C17)
N <- dim(C17)[1]
n

# probabilidad de selección de cada individuo
pi_k <- n/N

# factor de expansión: 1/pi_k
1/pi_k


# En nuestra muestra seleccionada: pi_k
muestra$pi_k <- n/N
head(muestra)

# En nuestra muestra seleccionada: factor de expansión
muestra$w <- 1/muestra$pi_k
head(muestra)

# volvamos a estimar, pero consideremos el factor de expansión
par_pet
sum(muestra$pet*muestra$w, na.rm = TRUE)

par_ocu
sum(muestra$ocu*muestra$w, na.rm = TRUE)

par_to
sum(muestra$ocu*muestra$w, na.rm = TRUE)/sum(muestra$pet*muestra$w, na.rm = TRUE)


##################
# pi_kl en este caso (MAS sin reemplazo) es simple de determinar ...
# pero entre más complejo el diseño más complejo su cálculo
# podemos calcular pi_kl o podemos usar un package que hace eso y más
##################

library(grid)
library(Matrix)
library(survival)
library(survey)

##################
# MAS sin reemplazo
##################
muestra.dsg <- svydesign(id=~1, weights=~w, data=muestra)


# volvamos a estimar, pero consideremos el factor de expansión
par_pet
sum(muestra$pet*muestra$w, na.rm = TRUE)
svytotal(~pet, muestra.dsg, na.rm = TRUE)

par_ocu
sum(muestra$ocu*muestra$w, na.rm = TRUE)
svytotal(~ocu, muestra.dsg, na.rm = TRUE)

par_to
sum(muestra$ocu*muestra$w, na.rm = TRUE)/sum(muestra$pet*muestra$w, na.rm = TRUE)
svyratio(~ocu, ~pet, muestra.dsg, na.rm = TRUE)
svymean(~ocu, muestra.dsg, na.rm = TRUE)


# Total poblacional por sexo
svytotal(~ factor(sexo), muestra.dsg, na.rm = TRUE)

# Total poblacional por region
svytotal(~ factor(region), muestra.dsg, na.rm = TRUE)

# Total poblacional por tramos de edad
svytotal(~ factor(gedad), muestra.dsg, na.rm = TRUE)


# Total poblacional por sexo
totalSexo <- svytotal(~ factor(sexo), muestra.dsg, na.rm = TRUE)

# funciones sobre el objeto guardado
totalSexo
coef(totalSexo)
SE(totalSexo)
cv(totalSexo)*100
confint(totalSexo)
confint(totalSexo, df = degf(muestra.dsg))


# estimación para subpoblaciones
svytotal(~ocu, subset(muestra.dsg, sexo == 2 & region == 1) , na.rm = TRUE)
cv(svytotal(~ocu, subset(muestra.dsg, sexo == 2 & region == 1), na.rm = TRUE))*100
confint(svytotal(~ocu, subset(muestra.dsg, sexo == 2 & region == 1), na.rm = TRUE), df = degf(subset(muestra.dsg, sexo == 2 & region == 1), na.rm = TRUE) )


# estimaciones más elaboradas
svyby(~ocu, ~region, muestra.dsg, svytotal, na.rm = TRUE)
cv(svyby(~ocu, ~region, muestra.dsg, svytotal, na.rm = TRUE))*100
confint(svyby(~ocu, ~region, muestra.dsg, svytotal, na.rm = TRUE), df = degf(muestra.dsg) )


###############
# una muestra probabilística con el diseño más simple (MAS sin reemplazo)
# svydesign(id=~1, weights=~w, data=muestra)

# CONGLOMERACION
# ESTRATIFICACION
# AJUSTES
# ....
###############
