
#______ kNN_______
#K-vecinos próximos

library(MASS)

# Cargar los datos iris

Z<-as.data.frame(iris)
colnames(Z)

# Definir la matriz de datos y la variable respuesta
# Con las clasificaciones
x<-Z[,1:4]
y<-Z[,5]

# Se definen las variables y observaciones
n<-nrow(x)
p<-ncol(x)

# Grafico scatter plot
# Creacion de un vector de colores
y
col.iris<-c("blue","green","orange")[y]
col.iris

pairs(x, main="Data set Iris, Setosa (azul),Versicolor (verde), Virginica (naranja)", 
      pch=19,col=col.iris)

#-----------------------
#         kNN
#-----------------------

library(class)

# Se fija una "semilla" para tener valores iguales
set.seed(1000)

# creacion de los ciclos
# para k=1 hasta k=20
# Selecciona el valor de k que tenga el error
# mas bajo.

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

knn.tables[[14]]
knn.tables[[18]]
knn.tables[[19]]


# el mas eficiente es k=14
# se señala el k mas eficiente
k.opt<-14

knn.cv.opt<-knn.class[[k.opt]]
knn.cv.opt

# tabla de contingencia con las clasificaciones buenas y malas
knn.tables[[k.opt]]

# cantidad de observaciones mal clasificadas
knn.mis[k.opt]

# Error de clasificacion (MR)
knn.mis[k.opt]/n

# Grafico de clasificaciones correctas y erroneas
col.knn.iris<-c("indianred1","black")[1*(y==knn.cv.opt)+1]
pairs(x, main="Clasificación kNN de Iris",
      pch=19, col=col.knn.iris)

#--------------------------------
# Ejercicio Penguins
#--------------------------------

#1.- Descargar la matriz penguins.
#2.- Copiar y pegar el script.
#3.- Adaptar el script. (elige la semilla)
#4.- Generar resultados (activar comandos)
#5.- Responder las siguientes preguntas:
#5.1.- ¿Cual es numero optimo de k-vecinos cercanos?
#5.2.- ¿Cual es la cantidad de observaciones mal clasificadas?
#5.3.- ¿Cual es el ratio de mala clasificación (MR)?
#6.- Generar el gráfico de buena y mala clasificacion. 


#______ kNN_______
#K-vecinos próximos: Matriz Penguins 
library(readxl)
penguins_1_ <- read_excel("C:/Users/Bonilla/Downloads/penguins (1).xlsx")
View(penguins_1_)

library(MASS)

# Cargar los datos iris

Z<-as.data.frame(penguins_1_)
colnames(Z)

# Definir la matriz de datos y la variable respuesta
# Con las clasificaciones
x<-Z[,4:5]
y<-Z[,2]

# Se definen las variables y observaciones
n<-nrow(x)
p<-ncol(x)

# Grafico scatter plot
# Creacion de un vector de colores
y
col.peng<- c("red", "blue", "green")
col.peng

pairs(x, main="Data set Penguins, Male(Blue), Female(Pink)", 
      pch=19, col = col.peng)

#-----------------------
#         kNN
#-----------------------

library(class)

# Se fija una "semilla" para tener valores iguales
set.seed(1001)

# creacion de los ciclos
# para k=1 hasta k=20
# Selecciona el valor de k que tenga el error
# mas bajo.

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

# el mas eficiente es k=6
# se señala el k mas eficiente
k.opt<-6

knn.cv.opt<-knn.class[[k.opt]]
knn.cv.opt

# tabla de contingencia con las clasificaciones buenas y malas
knn.tables[[k.opt]]

# cantidad de observaciones mal clasificadas
knn.mis[k.opt]

# Error de clasificacion (MR)
knn.mis[k.opt]/n

# Grafico de clasificaciones correctas y erroneas
col.knn.peng<-c("red", "blue", "green")[1*(y==knn.cv.opt)+1]
pairs(x, main="Clasificación kNN de Penguin",
      pch=19, col=col.knn.peng)

