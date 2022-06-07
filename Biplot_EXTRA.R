install.packages("datos")
library(datos)
datos::flores
View(flores)
str(flores)

#_______________ BIPLOT _____________-

# Instalacion de paquetes
install.packages("MultBiplotR")
library(MultBiplotR)

# Reconocimiento de la matriz de datos
load("Vinos.rda")

BD<-flores

#-------------------------------------
# Exploracion de matriz
#-------------------------------------
dim(BD)
str(BD)
colnames(BD)
attach(BD)

#---------------------------------
#  Graficos de exploracion
#----------------------------------
BX1<-BoxPlotPanel(BD, nrows=2, groups=BD$Especie)
BX1


#--------------------------------
#  Filtrado de variables
#--------------------------------

# 1.- Seleccion de variables numericas
X<-BD[,1:4]

# 2.- Generacion Plot
PL1<-plot(X[,1:4])

#----------------------------
#   Reduccion de la dimensionalidad
#-------------------------------

#1.- ACP
# Scaling= 
# 1: datos orginales, 
# 2: Resta la media global del conjunto de los datos, 
# 3: Doble centrado (agricultura / interaccion de resuduales)
# 4: Centrado por columnas (variables con misma escala)
# 5: Estandarizado por columnas 

acpflores<-PCA.Analysis(X,Scaling = 5)
summary(acpflores)

# Presentacion de tablas (markdown)
summary(acpflores, latex=TRUE)

#2.- Contenido del objeto acpvino
names(acpflores)

#3.- Generacion del grafico
# Sin caja
acp1<-plot(acpflores, ShowBox=FALSE)

# screeplot con barras 
acp2<-princomp(X, cor=TRUE, score=TRUE)
plot(acp2)

# Grafico circular de correlacion
acp3<-plot(acpflores, CorrelationCircle=TRUE, 
           ShowAxis=TRUE,  CexInd=1.5)

# agregar grupos al biplot
# definido por usuario
acpflores1<-AddCluster2Biplot(acpflores, ClusterType="us", 
                            Groups = BD$Especie)

# Grafico con poligonos
# CexInd= tamaño de los argumentos
acp4<-plot(acpflores1, PlotClus=TRUE, 
           ClustCenters=TRUE, margin=0.05, 
           CexInd=0.7, ShowBox=TRUE)

# grafico con elipses
acp5<-plot(acpflores1, PlotClus=TRUE, ClustCenters=TRUE, 
           margin=0.05, CexInd=0.7, TypeClus="el", 
           ShowBox=F)

# grafico con estrellas
acp6<-plot(acpflores1, PlotClus=TRUE, ClustCenters=TRUE, 
           margin=0.05, CexInd=0.7, TypeClus="st", 
           ShowBox=TRUE)


#------------------------------------
#  Biplot
#-------------------------------------

# alpha= 
#  0:GH
#  1:JK
#  2:HJ
# Predeterminado JK
bipflores<-PCA.Biplot(X, Scaling = 5)
summary(bipflores)

# Valores propios
bipflores$EigenValues
# screeplot
SC<-barplot(bipflores$EigenValues)

# Vectores propios
bipflores$EV

# Tabla de inercias
Inercias<-data.frame(paste("Eje",1:length(bipflores$EigenValues)),
                     bipvino$EigenValues, bipflores$Inertia, 
                     bipvino$CumInertia)

colnames(Inercias)<-c("Eje", "Valor Propio", 
                      "Inercia", "Inercia acumulada")

# Markdown
library(knitr)
kable(Inercias)

# tabla contribucion de columnas
kable(bipflores$ColContributions)

# Grafico
plot(bipflores, ShowBox=TRUE)

# Prolongacion de vectores linea recta
BP1<-plot(bipflores, mode="s", 
          margin=0.1, ShowBox=TRUE)

# Prolongacion de vectores con flechas y linea punteada
BP2<-plot(bipflores, mode="ah", margin=0.05, 
          ShowBox=TRUE)

# Grafico circular correlaciones 
GC<-CorrelationCircle(bipflores)

# Grafico contribuciones de los vectores
# Calidad de representacion eje 1, 2 y 1+2
ColContributionPlot(bipflores, AddSigns2Labs = FALSE)


# Proyeccion individuos sobre una variable 
# dp= selecciona la variable
BP3<-plot(bipflores, dp=2, mode="s", 
          ColorVar=c("blue", rep("grey",17)),
          ShowBox=TRUE)

#Proyeccion de ind sobre todas las variables
# PredPoints= individuo
BP4<-plot(bipflores, PredPoints=1, mode="s", 
          ColorVar=1:18, ShowBox=TRUE)

# Agregar cluster Jerarquico con datos originales
# metodo ward.D
bipvino=AddCluster2Biplot(bipflores, NGroups=4, 
                          ClusterType="hi", 
                          method="ward.D", 
                          Original=TRUE)

# Cluster aplicado al biplot
clusBP<-plot(bipflores, PlotClus=TRUE,ShowAxis=TRUE)
clusBP

