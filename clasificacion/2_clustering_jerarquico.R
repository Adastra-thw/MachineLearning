# Clusterting Jerárquico

#Paso 1: Importar los datos.
dataset = read.csv("Datasets/Mall_Customers.csv")
#Se seleccionan solament aquellas variables que se deben agrupar utilizando el mecanismo de agrupación (clustering) 
X = dataset[, 4:5]

#Paso 2: Utilizar el dendrograma para encontrar el número óptimo de clusters.
#Se utiliza la función "hclust" que precisamente permite crear un cluster jerárquico.
#La función recibe una matriz de distancias con el objetivo de determinar la distancia euclidiana entre cada observación del dataset.
#En el algoritmo de kmeans era necesario encontrar el número óptimo de centros para crear las agrupaciones. 
#En este caso dicho concepto no existe, por lo tanto lo que se hace para determinar el número de clusters es utilizar la varianza 
#de todos los puntos con un método conocido como ward
dendrogram = hclust(dist(X, 
                         method = "euclidean"),
                    method = "ward.D")

#Paso 3: Visualizar el dendrograma.
plot(dendrogram,
     main = "Dendrograma",
     xlab = "Clientes del centro comercial",
     ylab = "Distancia Euclidea")

#Paso 3.1: Análisis del dendrograma. 
# En este caso, es necesario determinar la línea de corte. Como norma general, las agrupaciones deben ser lo más homogeneas posibles.
# Para ello, se debe seleccionar la distancia más larga sin cortes.
# Hay un pequeño "truco para esto". Basta simplemente con extender todas las líneas horizontales que ha generado el dendrograma y a continuación, 
# selecciónar la línea vertical más larga sin cortes (es decir, la distancia eucldiana más larga).
# En el caso del ejemplo anterior es díficil verlo porque las líneas parecen muy iguales, pero se selecciona finalmente la distancia entre ~300 y ~900.
# Un vez seleccionada la distnacia eucledea más larga, se cuentan las líneas paralelas verticales a esta, lo que representa el número de clusters, 
# que en el caso de este ejemplo es 5. 


# Paso 4: Generar el cluster jerárquico como se ha hecho antes
hc = hclust(dist(X, method = "euclidean"), 
                    method = "ward.D")
# Con la función "cutree" se corta el árbol en el número de clusters determinado. 
# El resultado es un vector en donde se indica cada observación en qué cluster ha quedado (se verá un número de cluster entre 1 y 5 en éste caso)  
y_hc = cutree(hc, k=5)
#> y_hc
# [1] 1 2 1 2 1 2 1 2 1 2 1 2 1 2 1 2 1 2 1 2 1 2 1 2 1 2 1 2 1 2 1 2 1 2 1 2 1 2 1 2 1 2 1 3 1
# [46] 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3
# [91] 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 4 3 4 3 4 5 4 5 4 3 4 5
# [136] 4 5 4 5 4 5 4 3 4 5 4 3 4 5 4 5 4 5 4 5 4 5 4 5 4 3 4 5 4 5 4 5 4 5 4 5 4 5 4 5 4 5 4 5 4
# [181] 5 4 5 4 5 4 5 4 5 4 5 4 5 4 5 4 5 4 5 4

#Paso 5. Visualizar los clusters.
#Se utilizan las mismas instrucciones que se han visto para kmeans utilizando la librería de visualización "cluster".
#install.packages("cluster")
library(cluster)
clusplot(X, 
         y_hc,
         lines = 0,
         shade = TRUE,
         color = TRUE,
         labels = 2,
         plotchar = FALSE,
         span = TRUE,
         main = "Clustering de clientes",
         xlab = "Ingresos anuales",
         ylab = "Puntuación (1-100)"
)