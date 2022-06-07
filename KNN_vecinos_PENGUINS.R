
# Ejercicio Penguins
#- 1.- Descargar la matriz penguins.
#- 2.- Copiar y pegar el script.
#- 3.- Adaptar el script. (elige la semilla)
#- 4.- Generar resultados (activar comandos)
# 5.- Responder las siguientes preguntas:
# - 5.1.- ¿Cual es numero optimo de k-vecinos cercanos?
#  - 5.2.- ¿Cual es la cantidad de observaciones mal clasificadas?
#  - 5.3.- ¿Cual es el ratio de mala clasificación (MR)?
#  - 6.- Generar el gráfico de buena y mala clasificacion. 


#______ kNN_______
#K-vecinos próximos: Matriz Penguins 


library(readxl)
library(readxl)
penguins_1_ <- read_excel("PENGUIN/penguins (1).xlsx")
head(penguins_1_)

library(MASS)


# Cargar los datos Penguins 


Z<-as.data.frame(penguins_1_)
colnames(Z)

# Definir la matriz de datos y la variable respuesta
# Con las clasificaciones

x<-Z[,4:5]
y<-Z[,8]


# Se definen las variables y observaciones

n<-nrow(x)
p<-ncol(x)



# Grafico scatter plot
# Creacion de un vector de colores

head(y)
col.peng<- c("Blue", "pink")
col.peng

pairs(x, main="Data set Penguins, Male(Blue), Female(Pink)", 
      pch=19, col = col.peng)


# kNN



library(class)


# Se fija una "semilla" para tener valores iguales

set.seed(1002)

# Creación de los ciclos para k=1 hasta k=20

## Selecciona el valor de k que tenga el error mas bajo.

# Inicialización de una lista vacia de tamaño 20


knn.class<-vector(mode="list",length=20)
knn.tables<-vector(mode="list", length=20)


# Clasificaciones erroneas

knn.mis<-matrix(NA, nrow=20, ncol=1)
knn.mis

for(k in 1:20){
  knn.class[[k]]<-knn.cv(x,y,k=k)
  knn.tables[[k]]<-table(y,knn.class[[k]])
  # la suma de las clasificaciones menos las correctas
  knn.mis[k]<- n-sum(y==knn.class[[k]])
}

knn.mis


# Numero optimo de k-vecinos

which(knn.mis==min(knn.mis))

knn.tables[[6]]

# Nota 
#Al realizar el análisis encontramos que los valores k son k= 6 y 13, recordemos que se toma el menor para continuar con el análisis 

# Se señala el k mas eficiente

k.opt<-8

knn.cv.opt<-knn.class[[k.opt]]
head(knn.cv.opt)


# Tabla de contingencia con las clasificaciones buenas y malas

knn.tables[[k.opt]]



# Cantidad de observaciones mal clasificadas

knn.mis[k.opt]
# *Cantidad de observaciones mal clasificada = 55*

# Error de clasificacion (MR)

knn.mis[k.opt]/n

#*¿Cual es el ratio de mala clasificación (MR)? = 0.1598*

# Grafico de clasificaciones correctas y erroneas

col.knn.peng<-c( "blue", "pink")[1*(y==knn.cv.opt)+1]
pairs(x, main="Clasificación kNN de Penguin",
      pch=19, col=col.knn.peng)
