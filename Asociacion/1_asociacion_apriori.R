# Apriori

#Paso 1. Preprocesado de Datos e instalación de librería arules
install.packages("arules")
install.packages("arulesViz")
library(arules) 
library(arulesViz)

#En este caso el dataset no tiene cabeceras, por lo tanto se debe indicar el parámetro "header" a FALSE.
#Esto se hace para que no utilice el primer registro como nombre de las columnas.
#En este dataset en concreto cada fila representa una cesta de la compra. Clientes que han llegado a un supermercado
#y ha comprado los productos que aparecen en cada fila del dataset (cesta de la compra).
dataset = read.csv("Datasets/Market_Basket_Optimisation.csv", header = FALSE)

#dada la forma que tiene el dataset, en este caso el procesado de datos será distinto. 
#Se procede a crear una matriz de dispersión, la cual se encargará de organizar los valores de una forma más natural.
#La matriz se encargará de ubicar los valores del dataset original en las columnas (es decir, los productos de la cesta).
#Y cada fila de dicha matriz de dispersión será cada una de las transacciones (compras). 
# El valor de cada celda representará si en cada transacción se ha comprado o no el producto referenciado en la columna.
# En este caso, los productos duplicados se han eliminado con "rm.duplicates".
dataset = read.transactions("Datasets/Market_Basket_Optimisation.csv",
                            sep = ",", rm.duplicates = TRUE)

#Paso 2. Análisis de los datos.
#Con la función summary sobre el dataset generado, se podrán observar el número de columnas (productos de las compras).
#También se podrán ver otros detalles como los items más frecuentes en el conjunto de datos, por ejemplo:
#most frequent items:
#mineral water          eggs     spaghetti  french fries     chocolate       (Other) 
#1788                   1348          1306          1282          1229         22405 
#Significa que agua mineral y huevos son los productos que han aparecido con más frecuencia en las transacciones (compras).
#Por otro lado también se puede apreciar la longitud de la districión, indicando cuántos items se han incluido en cada transacción.
#Por ejemplo:
# element (itemset/transaction) length distribution:
#   sizes
# 1    2    3    4    5    6    7    8    9   10   11   12   13   14   15   16   18   19   20 
# 1754 1358 1044  816  667  493  391  324  259  139  102   67   40   22   17    4    1    2    1 
# 
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1.000   2.000   3.000   3.914   5.000  20.000 
#
# Significa que, por ejemplo, 1754 transacciones contenian solamente 1 producto, 1358 contenian 2 productos, etc.
# También indica que la media de los usuarios compra 3,914 items, la mediana 3 y el máximo 20 (campana de Gauss).
summary(dataset)

#Paso 3. Enseñar un plot con los items más frecuentes del dataset.
itemFrequencyPlot(dataset, topN = 10)

#Paso 4. Entrenar algoritmo Apriori con el dataset
#A continuación se utiliza el método "apriori" que se encuentra disponible en la librería "arules".
#Esta función solo necesita el dataset que en este caso es la matriz de dispersión y el argumento "parameter".
#Dicho argumento recibe una lista, en la cual es necesario indicar el valor mínimo de soporte y confianza.
#Estos dos valores son importantes para definir las reglas de asociación entre productos con unas condiciones mínimas.
#Para decidir qué valores indicar en estos parámetros puede ser útil ver el gráfico anterior sobre la frecuencia relativa de los items.
#Evidentemente, para que las reglas generadas sean más significativas y aporten valor al modelo, se seleccionan
#aquellos items que tienen la mayor frecuencia relativa. 
#Por ejemplo, el soporte podría ser aquellos productos que se compran al menos 3 veces cada día.
#Dado que el número de observaciones (transacciones) es de 7500, se procede a hacer el calculo: 3*7/7500 = 0.0028. 
#Dicho valor se puede redonderar, por ejemplo a 0.003 y este sería el soporte del modelo.
#Para el nivel de confianza, al ver la documentación de la función apriori, el valor por defecto es 0.8
#El problema de este nivel de confianza es que significa que las reglas creadas deben cumplirse 
#en al menos el 80% de las transacciones estudiadas, lo cual puede traducirse en que no se pueden generar reglas con un mínimo tan exigente.

rules = apriori(data = dataset, 
                parameter = list(support = 0.003, confidence = 0.6))

#Paso 5. Visualización de los resultados.
#Se debe aplicar el método "lift" que basicamente consiste en ordenar las reglas de mayor a menor dependiendo de su relevancia.
#La función "inspect" del paquete "arules" permite precisamente obtener dicho conjunto de reglas.
#inspect(rules[1:10]) #Se obtienen 10 reglas, pero no en orden de significancia.
#La función "sort" permite ordenar las reglas utilizando el método "lift", es decir, las más significativas primero.
inspect(sort(rules, by = 'lift')[1:10])

plot(rules, method = "graph", engine = "htmlwidget")


