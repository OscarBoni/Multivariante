#Instalar paqueterias 

install.packages("psych")
library(psych)
library(polycor)
install.packages("ggcorrplot")
library(ggcorrplot)

# Extracción de datos 
x <- bfi

# Exploración de matriz
dim(x)

# Tipos de variables
str(x)

# Nombre de las variables
colnames(x)

# Creación de una nueva matriz
x1 <- bfi[1:200, 1:25]

# Matriz de correlaciones
R <- hetcor(x1)$correlations

# Gráfico de correlaciones
ggcorrplot(R, type = "lower", hc.order = TRUE)

# Factorización de la matriz de correlaciones
p_Barlett <- cortest.bartlett(R)

# Visualización de p-valor
p_Barlett$p.value

#Ho: Las variables están correlacionadas
#Ha: Las variables no están correlacionadas.

## Criterio Kaiser-Mayer 
#0,00 a 0.49 No adecuados
#0.50 A 0.59 Poco adecuados 
#0.60 a 0.69 Aceptables
#0.70 a 0.89 Buenos
#0.80 a 1.00 Excelentes

KMO(R)

#Extracción de factores

#minres: mínim residuo
#mle: max Verosimilitud
#paf: ejes principales 
#alpha: alfa
#minchi: minimos cuadrados
#minrak: minimo rango 

modelo1 <- fa(R, nfactor = 3, rotate = "none", fm = "mle")

modelo2 <- fa(R, nfactor = 3, rotate = "none", fm = "minres")


C1 <- sort(modelo1$communality, decreasing = TRUE)

C2 <- sort(modelo2$communality, decreasing = TRUE)

head(cbind(C1, C2))

# Extracción de unicidades
u1 <- sort(modelo1$uniquenesses, decreasing = TRUE)

u2 <- sort(modelo2$uniquenesses, decreasing = TRUE)

head(cbind(u1,u2))

# Generación de screeplot
scree(R)

# Rotación de la matriz
install.packages("GPArotation")
library(GPArotation) 


rot <- c("None", "Varimax", "Quartimax", "Promax")
bi_mod <- function(tipo){
  biplot.psych(fa(x1, nfactors = 2,
                  fm= "minres", rotate = tipo), 
               main = paste("Biplot con rotación", tipo), 
               col = c(2,3,4), pch = c(21,18), group = bfi[,"gender"])
}


sapply(rot,bi_mod)


#Grafico de arbol 

modelo_varimax <- fa(R, nfactors = 5,
                     rotate = "varimax",
                     fm = "minres")

fa.diagram(modelo_varimax)


# Vizualización de la matriz de carga rotada 

print(modelo_varimax$loadings, cut= 0)
