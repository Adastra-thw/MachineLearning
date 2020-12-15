# Eclat

#Paso 1. Preprocesado de Datos
#install.packages("arules")
library(arules)
dataset = read.csv("Datasets/Market_Basket_Optimisation.csv", header = FALSE)
dataset = read.transactions("Datasets/Market_Basket_Optimisation.csv",
                            sep = ",", rm.duplicates = TRUE)
summary(dataset)
itemFrequencyPlot(dataset, topN = 10)

#Paso 2. Entrenar algoritmo Eclat con el dataset
#En este caso el algoritmo de eclat se encargará de realizar los calculos en función a grupos, en lugar de hacerlo sobre items
#Apriori requiere de un nivel de soporte y confianza mínimo, en el casl de eclat, solo es necesario el nivel de soporte
#mientras que el nivel de confianza no se especifica. No obstante, es importante indicar de cuántos elementos estará compuesto el grupo.
#El parametro "minlen" permite establecer el número de elementos que debe contener cada regla de asociación
#generada por el algoritmo "eclat", es decir, que los grupos de items asociados deben estar compuestos por un valor mínimo X.
rules = eclat(data = dataset, 
              parameter = list(support = 0.003, minlen = 2))

# Paso 3. Visualización de los resultados
inspect(sort(rules, by = 'support')[1:10])



