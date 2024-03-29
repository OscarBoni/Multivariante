
#Analisis de componentes principales
x<-as.data.frame(state.x77)

#2.- Quitar los espacios de los nombres
colnames(x)[4]="Life.Exp"
colnames(x)[6]= "HS.Grad"

# 3.- Se definen n (numero de estados) y p (variables)
dim(x)

n<-dim(x)[1]
p<-dim(x)[2]

# 4.- Generaci�n de un scatterplot
# de las variables originales
pairs(x,col="blue", pch=19, 
      main="Variables originales")

#5.- Obtenci�n de los componentes principales
# con base en la matriz de covarianza muestral
mu<-colMeans(x)
s<-cov(x)

#6.- Obtenci�n de los componentes principales 
# con base a la matriz de covarianza muestral
es<-eigen(s)
es

# 7.- Matriz de auto-valores
eigen.val<-es$values

# 8.- Matriz de auto-vectores
eigen.vec<-es$vectors

# Proporci�n de variabilidad para cada vector
pro.var<-eigen.val/sum(eigen.val)

# Proporci�n de variabilidad acumulada
pro.var.acum<-cumsum(eigen.val)/sum(eigen.val)

#----------------------------------
# Obtencion de los componentes principales con
# base en la matriz de correlaciones muestrales
#---------------------------------------

R<-cor(x)
eR<-eigen(R)
eR

# Obtenci�n de auto-valores
eigen.val<-eR$values

# Obtenci�n de auto-vectores
eigen.vec<-eR$vectors

# Proporcion de variablidad
pro.var<-eigen.val/sum(eigen.val)

# Proporcion de variabilidad acumulada
pro.var.acum<-cumsum(eigen.val)/sum(eigen.val)

# Media de los auto-valores
mean(eigen.val)

#---------------------------
# Obtencion de los coeficientes (nuevas variables)
# 
#--------------------------

# 1.- Centrar los datos con respecto a la media
ones<-matrix(rep(1,n),nrow=n, ncol=1)

# 2.- Construccion de la matriz centrada
X.cen<-as.matrix(x)-ones%*%mu
X.cen

# 3.- Construccion de la matriz diagonal de las 
# varianzas
Dx<-diag(diag(s))
Dx

# 4.- Construccion de la matriz centrada multiplicada
# por Dx^1/2

Y<-X.cen%*%solve(Dx)^(1/2)
Y #datos normalizados

# 5.- Construccion de los coeficientes o scores
# eigen.vec matriz de autovectores
scores<-Y%*%eigen.vec

# Nombramos las columnas PC1...PC8
colnames(scores)<-c("PC1","PC2","PC3","PC4","PC5",
                    "PC6", "PC7","PC8")

# visualizamos
scores

# Generacion del grafico de los scores
pairs(scores, main="scores", col="blue", pch=19)

