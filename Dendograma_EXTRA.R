# Dendrograma
# Cargamos librerias
install.packages("cluster.datasets")
library("cluster.datasets")

# Bajamos la matriz de datos
data("languages.spoken.europe")
view(languages.spoken.europe)

# Cambiamos el nombre de la matriz
LSE=languages.spoken.europe
head(LSE)

#Exploración de la matriz 
dim(LSE)
str(LSE)
anyNA(LSE)

# Calculo de la matriz de distancia de Mahalonobis

dist.LSE<-dist(LSE[,2:12])

# Convertir los resultados del Calculo de la distancia a una matriz de datos y me indique 3 digitos.
round(as.matrix(dist.LSE)[1:12, 1:12],3)

# Calculo del dendrograma
dend.LSE<-as.dendrogram(hclust(dist.LSE))

# Generacion del dendrograma
plot(dend.LSE)

# Agregar etiquetas al Grafico
LSE.nombres=LSE
rownames(LSE.nombres)= LSE.nombres$country
LSE.nombres=LSE.nombres[,-1]

# Construimos de nuevo el Grafico
plot(as.dendrogram(hclust(dist(LSE.nombres))))

# Interpretación idiomas 
#En nuestro dendograma podemos decir que se divide en 3 clusters principales
#esto quiere decir que entre nuestros 3 clusters existirán paises que comparten los
#mismos idiomas, por ejemplo, si creamos grupos podriamos decir que en el grupo 1 
#el cual incluye (alemania del este, Austria, Luxenburgo y Suiza) el idioma mas hablado 
#es el aleman, mientras que en el grupo 2 (DInamarca, Noruega,Suecia, Gran Bretaña e Irlanda) aunque 
#tambien está presente el alemán predomina mas el inglés. 

#  Modificar el dendrograma
install.packages("dendextend")
library(dendextend)

# Guardar las etiquetas en un objeto "L"
L=labels(dend.LSE)
labels(dend.LSE)=LSE$country[L]

# Cambiar el tama?o de las etiquetas
dend.LSE %>%
  set(what="labels_col", "red") %>% #Colores etiqueta
  set(what="labels_cex", 0.8) %>%
  plot(main="Dendrograma de Idiomas en Europa")

# Dendograma de Circulo
install.packages("circlize")
library("circlize")

circlize_dendrogram(dend.LSE,labels_track_height=NA,
                    dend_track_height=0.1)
