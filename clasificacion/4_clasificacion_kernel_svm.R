# Kernel SVM

#Paso 1. Importar el dataset
dataset = read.csv('Datasets/Social_Network_Ads.csv')
dataset = dataset[, 3:5]

#Paso 2. Dividir los datos en conjunto de entrenamiento y conjunto de test
# install.packages("caTools")
library(caTools)
set.seed(123)
split = sample.split(dataset$Purchased, SplitRatio = 0.75)
training_set = subset(dataset, split == TRUE)
testing_set = subset(dataset, split == FALSE)

#Paso 3. Escalado de valores
training_set[,1:2] = scale(training_set[,1:2])
testing_set[,1:2] = scale(testing_set[,1:2])

#Paso 4. Ajustar el clasificador con el conjunto de entrenamiento.
#install.packages("e1071")
#Los parámetros del clasificador SVM en este caso cambia un poco en el kernel usado.
#Si los datos no se pueden clasificar linealmente, se puede emplear otro kernel que permita generar otros tipos de clasificaciones, entre los que se incluyen gausianos, sigmoides, polinomicos, radiales, etc.
library(e1071)
classifier = svm(formula = Purchased ~ .,
                 data = training_set, 
                 type = "C-classification",
                 kernel = "radial")

#Paso 5. Predicción de los resultados con el conjunto de testing
#Se procede a generar las predicciones utilizando el clasificador con kernel radial y el conjunto de datos de testing.
y_pred = predict(classifier, newdata = testing_set[,-3])
#Se puede ver en la consola los resultados de la predicción y comparar (un vistazo rápido) si coindicen con los datos del conjunto de testing. 
#> y_pred
#2   4   5   9  12  18  19  20  22  29  32  34  35  38  45  46  48  52  66  69  74  75  82  84 
#0   0   0   0   0   1   1   1   0   0   1   0   0   0   0   0   0   0   0   0   1   0   0   0 
#------


#Paso 6. Crear la matriz de confusión
cm = table(testing_set[, 3], y_pred)
#Igual que en los casos anteriores.
#> cm
#y_pred
#       0  1
#0      58  6
#1      4 32
#Según la matriz de confusión el 90% de las predicciones fueron acertadas.

#Paso 7. Visualización del conjunto de entrenamiento
#install.packages("ElemStatLearn")
library(ElemStatLearn)
set = training_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('Age', 'EstimatedSalary')
y_grid = predict(classifier, newdata = grid_set)
plot(set[, -3],
     main = 'SVM Kernel (Conjunto de Entrenamiento)',
     xlab = 'Edad', ylab = 'Sueldo Estimado',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))


#Paso 7.1. Visualización del conjunto de testing
set = testing_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('Age', 'EstimatedSalary')
y_grid = predict(classifier, newdata = grid_set)
plot(set[, -3],
     main = 'SVM Kernel (Conjunto de Testing)',
     xlab = 'Edad', ylab = 'Sueldo Estimado',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))
