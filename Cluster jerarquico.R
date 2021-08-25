datos <- read.delim("clipboard")
View(datos)
datos$region
table(datos$region)

#Filtramos solo los datos de America 
library(dplyr)
America=datos %>%
  filter(region=="Americas")
America=America[c(1,4,5)]
America

# Gráfico/Dispersión de puntos de las variables
plot(America$adfert, America$school, pch = 21,
     bg = "red",  # Color de fondo
     cex = 1.5)   # Tamaño del símbolo

# Asignamos las etiquetas
text(America$adfert, America$school,
     labels = America$country,
     cex = 1,  # Tamaño del símbolo
     pos = 1,  # posición de etiqueta
     col = "blue")


# AQUI EMPIEZA EL CÓDIGO DE CLUSTER JERARQUICO (DE AGRUPAMIENTO)

#-------------------- MÉTODO SINGLE-------------------------------------
# Cálculo de la distancia euclídea
matriz.dist <- dist(America[-1] ,method="euclidean",diag=TRUE)

# Efectuamos el cluster con el método vecino más cernano (single)
hclust.single <- hclust(matriz.dist, method ="single")

# Dendograma
plot(hclust.single, labels = America$country)

# Historial de conglomeración
data.frame(hclust.single[2:1])



#------------------- MÉTODO COMPLETE----------------------------------
# Cálculo de la distancia euclídea
matriz.dist_2 <- dist(America[-1] ,method="euclidean",diag=TRUE)

# Efectuamos el cluster con el método vecino más lejano (complete)
hclust.complete <- hclust(matriz.dist_2, method ="complete")

# Dendograma
plot(hclust.complete, labels = America$country)

# Historial de conglomeración
data.frame(hclust.complete[2:1])



#--------------------- MÉTODO CENTROIDE---------------------------------
# Cálculo de la distancia euclídea
matriz.dist_3 <- dist(America[-1] ,method="euclidean",diag=TRUE)

# Efectuamos el cluster con el método del centroide (centroid)
hclust.centroid <- hclust(matriz.dist_3, method ="centroid")

# Dendograma
plot(hclust.centroid, labels = America$country)

# Historial de conglomeración
data.frame(hclust.centroid[2:1])



#------------------------- MÉTODO AVERAGE-------------------------------
# Cálculo de la distancia euclídea
matriz.dist_4 <- dist(America[-1] ,method="euclidean",diag=TRUE)

# Efectuamos el cluster con el método promedio entre clusters (average)
hclust.average <- hclust(matriz.dist_4, method ="average")

# Dendograma
plot(hclust.average, labels = America$country)

# Historial de conglomeración
data.frame(hclust.average[2:1])


#-------------------------- MÉTODO WARD. D ------------------------------
# Cálculo de la distancia euclídea
matriz.dist_5 <- dist(America[-1] ,method="euclidean",diag=TRUE)

# Efectuamos el cluster con el método promedio entre clusters (average)
hclust.ward <- hclust(matriz.dist_5, method ="ward.D") # ward equivale a ward.D

# Dendograma
plot(hclust.ward, labels = America$country)
# Historial de conglomeración
data.frame(hclust.ward[2:1])

#compararemos los diferentes metodos de agrupación 
#con la función Agnes de la libreria Cluster 

#Vector de metodo de comparación 
m <- c( "average", "single", "complete", "ward")
names(m) <- c( "average", "single", "complete", "ward")

# función de comparación 
library(cluster) # para la función agnes
ac <- function(x) {
  agnes(America[-1], method = x)$ac
}
library(purrr) # para la función map_dbl
map_dbl(m, ac)

# índices de parada con cada método 
library(NbClust)
res1 <- NbClust(America[-1], distance ="euclidean", min.nc=2, max.nc=6, method ="single",index ="alllong")

res2 <- NbClust(America[-1], distance ="euclidean", min.nc=2, max.nc=6, method ="complete",index ="alllong")

res3 <- NbClust(America[-1], distance ="euclidean", min.nc=2, max.nc=6, method ="centroid",index ="alllong")

res4 <- NbClust(America[-1], distance ="euclidean", min.nc=2, max.nc=6, method ="average",index ="alllong")

res5 <- NbClust(America[-1], distance ="euclidean", min.nc=2, max.nc=6, method ="ward.D",index ="alllong")

