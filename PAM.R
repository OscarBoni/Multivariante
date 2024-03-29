
#_______ PARTITION AROUND MEDOIDS (PAM)_____

library(cluster)

# Cargar la matriz de datos.

X<-as.data.frame(state.x77)
colnames(X)

#-------------------------------------
#     Transformacion de datos
#-------------------------------------

#1.- Transformacion de las variables x1,x3 y x8
# con la funcion de logaritmo.

X[,1]<-log(X[,1])
colnames(X)[1]<-"Log-Population"

X[,3]<-log(X[,3])
colnames(X)[3]<-"Log-Illiteracy"

X[,8]<-log(X[,8])
colnames(X)[8]<-"Log-Area"

#---------------------------------
#    Metodo PAM
#---------------------------------

#1.- Separacion de filas y columnas.

dim(X)
n<-dim(X)[1]
p<-dim(X)[2]

# 2.- Estandarizacion univariante.
X.s<-scale(X)

# 3.- Aplicacion del algoritmo
pam.3<-pam(X.s,3)

# 4.- Clusters
cl.pam<-pam.3$clustering
cl.pam

#5.- Scatter plot de la matriz con los grupos
col.cluster<-c("blue","red","green")[cl.pam]
pairs(X.s, col=col.cluster, main="PAM", pch=19)

#---------------------------------
#  Visualizacion con Componentes Principales
#----------------------------------
clusplot(X.s,cl.pam)
text(princomp(X.s)$scores[,1:2],
     labels=rownames(X.s),pos=1, col="blue")

#-------------------------------------
#   Silhouette
#-------------------------------------

# Representacion grafica de la eficacia de
# clasificacion de una observacion dentro de un
# grupo.

# 1.- Generacion de los calculos
dist.Euc<-dist(X.s, method = "euclidean")
Sil.pam<-silhouette(cl.pam, dist.Euc)

#2.- Generacion del grafico
plot(Sil.pam, main="Silhouette for PAM", 
     col="blue")




# 2 sugerir un nuevo numero de clusters

# 3.- Aplicacion del algoritmo
pam.3<-pam(X.s,7)

# 4.- Clusters
cl.pam<-pam.3$clustering
cl.pam

#5.- Scatter plot de la matriz con los grupos
col.cluster<-c("blue","red","green", "yellow", "black")[cl.pam]
pairs(X.s, col=col.cluster, main="PAM", pch=19)

#---------------------------------
#  Visualizacion con Componentes Principales
#----------------------------------
clusplot(X.s,cl.pam)
text(princomp(X.s)$scores[,1:2],
     labels=rownames(X.s),pos=1, col="orange")

#-------------------------------------
#   Silhouette
#-------------------------------------

# Representacion grafica de la eficacia de
# clasificacion de una observacion dentro de un
# grupo.

# 1.- Generacion de los calculos
dist.Euc<-dist(X.s, method = "euclidean")
Sil.pam<-silhouette(cl.pam, dist.Euc)

#2.- Generacion del grafico
plot(Sil.pam, main="Silhouette for PAM", 
     col="red")

