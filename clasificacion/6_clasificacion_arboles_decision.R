# Clasificación con Árboles de Decisión

#Paso 1. Importar el dataset
dataset = read.csv('Datasets/Social_Network_Ads.csv')
dataset = dataset[, 3:5]

#Paso 2. Codificar la variable de clasificación como factor
dataset$Purchased = factor(dataset$Purchased, levels = c(0,1))

#Paso 3. Dividir los datos en conjunto de entrenamiento y conjunto de test
# install.packages("caTools")
library(caTools)
set.seed(123)
split = sample.split(dataset$Purchased, SplitRatio = 0.75)
training_set = subset(dataset, split == TRUE)
testing_set = subset(dataset, split == FALSE)

#Paso 4. Ajustar el clasificador con el conjunto de entrenamiento.
#Se debe utilizar la librería rpart (recursive partitioning and regression tree) tal como se ha visto en los ejemplos de regresión, 
# sin embargo en este caso se utilizará para clasificación de datos.
#install.packages("rpart")
library(rpart)
classifier = rpart(formula = Purchased ~ ., 
                   data = training_set)

#Paso 5. Con el clasificador, que representa el entrenamiento que se ha realizado con los datos de training y el modelo seleccionado (rpart) 
# se procede a predecir los resultados con el conjunto de testing
# Como resulta evidente, para la predición se debe eliminar la columna correspondiente a la variable dependiente (Purchased). 
y_pred = predict(classifier, newdata = testing_set[,-3], type = "class")
#NOTA: Ver qué pasa si no se incluye el parámetro type= "class". Este parámetro asigna una clasificación lógica (1 o 0)
#y_pred = predict(classifier, newdata = testing_set[,-3])
#Resulta conveniente ver la variable "y_pred" que como se podrá comprobar tiene un valor distinto para cada observación y no hay "0" y "1".
#en su lugar, cada observación enseña la probabilidad de estar en el factor 0 o 1. 
# > y_pred
# 0          1
# 2   0.96703297 0.03296703
# 4   0.96703297 0.03296703
# 5   0.96703297 0.03296703


#Paso 6. Crear la matriz de confusión
cm = table(testing_set[, 3], y_pred)
#> cm
# y_pred
# 0  1
# 0 56  8
# 1  6 30

#Paso 7. Visualización del conjunto de entrenamiento
set = training_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.05)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 250)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('Age', 'EstimatedSalary')
y_grid = predict(classifier, newdata = grid_set, type = "class")
plot(set[, -3],
     main = 'Árbol de Decisión (Conjunto de Entrenamiento)',
     xlab = 'Edad', ylab = 'Sueldo Estimado',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))


#Paso 7.1 Visualización del conjunto de testing
set = testing_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.05)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 250)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('Age', 'EstimatedSalary')
y_grid = predict(classifier, newdata = grid_set, type = "class")
plot(set[, -3],
     main = 'Árbol de Clasificación (Conjunto de Testing)',
     xlab = 'Edad', ylab = 'Sueldo Estimado',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))

#Paso 8 (opcional). Representación visual del árbol de clasificación que se ha aplicado con la función rpart.
#plot(classifier)
#text(classifier)

