# Clustering con K-means

#Paso 1. Importar los datos
dataset = read.csv("Datasets/Mall_Customers.csv")

#Se seleccionan solamente aquellas variables que se deben agrupar utilizando el mecanismo de agrupación (clustering) 
X = dataset[, 4:5]

#Paso 2. Método del codo
#El metodo del codo representa la formula estadística WCSS (Within-Cluster-Sum-of-Squares) que permite encontrar el número de centroides (agrupaciones) óptimo.
#Se trata de un mecanismo que suma las distancias de cada observacion con respecto a su correspondiente centroide y las eleva al cuadrado.
#El objetivo es encontrar cuál es número de centroides que seleccionados al azar, aporta un número adecuado de observaciones clasificadas o agrupadas.
set.seed(3333)
wcss = vector()
for (i in 1:10){
  wcss[i] <- sum(kmeans(X, i)$withinss)
}

#Paso 3. Visualizar el método del codo para ver el valor adecuado para el algoritmo kmeans
plot(1:10, wcss, type = 'b', main = "Método del codo",
     xlab = "Número de clusters (k)", ylab = "WCSS(k)")

#Paso 4. Aplicar el algoritmo de k-means con k óptimo generado por el método del codo, que en este caso, tal como se pudo ver en el gráfico anterior es 5.
set.seed(33442)
kmeans <- kmeans(X, 5, iter.max = 300, nstart = 10)

kmeans$cluster

#Paso 5. Visualización de los clusters.
#La librería cluster permite visualizar las agrupaciones de clusters generadas por algoritmos como k-means.
#install.packages("cluster")
library(cluster)
clusplot(X, #Dataframe con todas las observaciones
         kmeans$cluster, #Enseña los clusters a los que pertenece cada punto.
         lines = 0, #No unir por líneas los puntos.
         shade = FALSE, #Marcar los clusters.
         color = TRUE, #Pintar colores.
         labels = 2, #Etiquetar los puntos. Ver la documentación para ver cómo se etiqueta el plot.
         plotchar = TRUE, #Enseñar símbolos diferentes para cada cluster (no)
         span = TRUE, #Aparece cada cluster representado con una elipse.
         main = "Clustering de clientes",
         xlab = "Ingresos anuales",
         ylab = "Puntuación (1-100)"
         )