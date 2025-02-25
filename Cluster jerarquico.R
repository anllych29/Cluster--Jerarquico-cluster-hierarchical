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

# Gr�fico/Dispersi�n de puntos de las variables
plot(America$adfert, America$school, pch = 21,
     bg = "red",  # Color de fondo
     cex = 1.5)   # Tama�o del s�mbolo

# Asignamos las etiquetas
text(America$adfert, America$school,
     labels = America$country,
     cex = 1,  # Tama�o del s�mbolo
     pos = 1,  # posici�n de etiqueta
     col = "blue")


# AQUI EMPIEZA EL C�DIGO DE CLUSTER JERARQUICO (DE AGRUPAMIENTO)

#-------------------- M�TODO SINGLE-------------------------------------
# C�lculo de la distancia eucl�dea
matriz.dist <- dist(America[-1] ,method="euclidean",diag=TRUE)

# Efectuamos el cluster con el m�todo vecino m�s cernano (single)
hclust.single <- hclust(matriz.dist, method ="single")

# Dendograma
plot(hclust.single, labels = America$country)

# Historial de conglomeraci�n
data.frame(hclust.single[2:1])



#------------------- M�TODO COMPLETE----------------------------------
# C�lculo de la distancia eucl�dea
matriz.dist_2 <- dist(America[-1] ,method="euclidean",diag=TRUE)

# Efectuamos el cluster con el m�todo vecino m�s lejano (complete)
hclust.complete <- hclust(matriz.dist_2, method ="complete")

# Dendograma
plot(hclust.complete, labels = America$country)

# Historial de conglomeraci�n
data.frame(hclust.complete[2:1])



#--------------------- M�TODO CENTROIDE---------------------------------
# C�lculo de la distancia eucl�dea
matriz.dist_3 <- dist(America[-1] ,method="euclidean",diag=TRUE)

# Efectuamos el cluster con el m�todo del centroide (centroid)
hclust.centroid <- hclust(matriz.dist_3, method ="centroid")

# Dendograma
plot(hclust.centroid, labels = America$country)

# Historial de conglomeraci�n
data.frame(hclust.centroid[2:1])



#------------------------- M�TODO AVERAGE-------------------------------
# C�lculo de la distancia eucl�dea
matriz.dist_4 <- dist(America[-1] ,method="euclidean",diag=TRUE)

# Efectuamos el cluster con el m�todo promedio entre clusters (average)
hclust.average <- hclust(matriz.dist_4, method ="average")

# Dendograma
plot(hclust.average, labels = America$country)

# Historial de conglomeraci�n
data.frame(hclust.average[2:1])


#-------------------------- M�TODO WARD. D ------------------------------
# C�lculo de la distancia eucl�dea
matriz.dist_5 <- dist(America[-1] ,method="euclidean",diag=TRUE)

# Efectuamos el cluster con el m�todo promedio entre clusters (average)
hclust.ward <- hclust(matriz.dist_5, method ="ward.D") # ward equivale a ward.D

# Dendograma
plot(hclust.ward, labels = America$country)
# Historial de conglomeraci�n
data.frame(hclust.ward[2:1])

#compararemos los diferentes metodos de agrupaci�n 
#con la funci�n Agnes de la libreria Cluster 

#Vector de metodo de comparaci�n 
m <- c( "average", "single", "complete", "ward")
names(m) <- c( "average", "single", "complete", "ward")

# funci�n de comparaci�n 
library(cluster) # para la funci�n agnes
ac <- function(x) {
  agnes(America[-1], method = x)$ac
}
library(purrr) # para la funci�n map_dbl
map_dbl(m, ac)

# �ndices de parada con cada m�todo 
library(NbClust)
res1 <- NbClust(America[-1], distance ="euclidean", min.nc=2, max.nc=6, method ="single",index ="alllong")

res2 <- NbClust(America[-1], distance ="euclidean", min.nc=2, max.nc=6, method ="complete",index ="alllong")

res3 <- NbClust(America[-1], distance ="euclidean", min.nc=2, max.nc=6, method ="centroid",index ="alllong")

res4 <- NbClust(America[-1], distance ="euclidean", min.nc=2, max.nc=6, method ="average",index ="alllong")

res5 <- NbClust(America[-1], distance ="euclidean", min.nc=2, max.nc=6, method ="ward.D",index ="alllong")

