
##Obtención de los datos
#Los datos utilizados para este ejemplo fueron obtenidos de la página kaggle 

#https://www.kaggle.com/datasets/bertiemackie/sloth-species

# Cargamos la base de datos 

# Descripción
#Esta base cuenta con datos sobre direntes especies de perezosos, de igual manera, 
#contiene datos como largo de la garra, peso, largo de cola, etc.

# Preparación de la matriz 
library(readr)
perezoso <- read_csv("C:/Users/Bonilla/Desktop/perezoso.csv")

head(perezoso)

# ANALISIS DISCRIMINANTE LINEAl
z <- as.data.frame(perezoso)

# Se define la matriz de datos y la variable
x<-z[,1:2]

y<-z[,5]

# Definir como n y p el numero de flores y variables
n<-nrow(x)
p<-ncol(x)

# Se aplica el Analisis discriminante lineal (LDA)
# Cross validation (cv): clasificacion optima
lda.pere<-lda(y~.,data=x, CV=TRUE)

# lda.iris$class contiene las clasificaciones hechas por CV usando LDA.
lda.pere$class

# Creacion de la tabla de clasificaciones buenas y malas
table.lda<-table(y,lda.pere$class)
table.lda

. 

# Proporcion de errores
mis.pere<- n-sum(y==lda.pere$class)
mis.pere/n



# scater plot
# Buenas clasificaciones en negro y malas en rojo
col.lda.pere<-c("indianred1","black")[1*(y==lda.pere$class)+1]
pairs(x,main="Buena Clasificacion (negro), Mala Clasificacion (rojo)",
      pch=19,col=col.lda.pere)


# Probabilidad de pertenencia a uno de los 2 grupos
lda.pere$posterior

# Grafico de probabilidades

plot(1:n, lda.pere$posterior[,1],
     main="Probabilidades a posterior",
     pch=20, col="cyan",
     xlab="Numero de observaciones", ylab="Probabilidades")
points(1:n,lda.pere$posterior[,2],
       pch=20, col="green")

