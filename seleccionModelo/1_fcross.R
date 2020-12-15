# k-fold cross validation

#Paso 1. Importar el dataset. #Se aplicará el mecanismo F-Cross validation sobre el modelo Kernel SVM. El código se basa en el mismo ejemplo visto antes para SVM.  
dataset = read.csv('Datasets/Social_Network_Ads.csv'); dataset = dataset[, 3:5]

#Paso 2. Dividir los datos en conjunto de entrenamiento y conjunto de test
# install.packages("caTools")
library(caTools); set.seed(123); split = sample.split(dataset$Purchased, SplitRatio = 0.75)
training_set = subset(dataset, split == TRUE); testing_set = subset(dataset, split == FALSE)

#Paso 3. Escalado de valores
training_set[,1:2] = scale(training_set[,1:2]); testing_set[,1:2] = scale(testing_set[,1:2])

#Paso 4. Ajustar el clasificador con el conjunto de entrenamiento.
#install.packages("e1071")
library(e1071); classifier = svm(formula = Purchased ~ ., data = training_set, type = "C-classification", kernel = "radial")

#Paso 5. Predicción de los resultados con el conjunto de testing
y_pred = predict(classifier, newdata = testing_set[,-3])

#Paso 6. Crear la matriz de confusión
cm = table(testing_set[, 3], y_pred)

#Paso 7. Aplicar algoritmo de k-fold cross validation.
#Se debe dividir el conjunto de entrenamiento en 10 bloques, dicho valor es arbitrario y se puede cambiar en el parámetro "k".
#A continuación se utiliza la función "lapply" la cual recibe los bloques creados anteriormente con la librería caret y una función anónima o lambda.
# La función lapply ejecutará la función anónima tantas veces como bloques se le envie al primer argumento (flods, que en este caso son 10).
# Esto signfica que que dicha función se ejecutará lo que se le indique 10 veces, que en este caso será el modelo SVM y así se aplica la técnica F-Cross. 
# Dentro de la función anónima, se obtiene el conjunto de entrenamiento completo excluyendo el bloque, tal como se ve en la imagen de la diapositiva
# y el conjunto de prueba será simplemente el bloque entero. A continuación se ejecuta el SVM, la predicción y la tabla CM como se ha visto anteriormente.
# No obtante, para cada ejecución del bloque se debe calcular la presición, para ello se utiliza la matriz de confusión de la siguiente forma:
# accuracy = Suma Predicciones Correctas/Total de Predicciones.
# Dicho calculo es precisamente lo que debe devolver la función. 
install.packages("caret")
library(caret)
folds = createFolds(training_set$Purchased, k = 10)
cv = lapply(folds, function(x) { 
  training_fold = training_set[-x, ]
  test_fold = training_set[x, ]
  classifier = svm(formula = Purchased ~ .,
                   data = training_fold, 
                   type = "C-classification",
                   kernel = "radial")
  y_pred = predict(classifier, newdata = test_fold[,-3])
  cm = table(test_fold[, 3], y_pred)
  accuracy = (cm[1,1]+cm[2,2])/(cm[1,1]+cm[1,2]+cm[2,1]+cm[2,2])
  return(accuracy)  
})

#Paso 8. Calculo de la media de las predicciones utilizando el método F-Cross.
#Se debe castear la lista como valores númericos y aplicar la media sobre dicha lista. 
#El valor resultante de dicha media es el nivel de fiabilidad del modelo aplicado y aporta un rendimiento que en este caso es bastante elevado.
#También resulta conveniente calcular el nivel de varianza (desviación estándar) para saber si entre cada bloque hay cambios grandes.
accuracy = mean(as.numeric(cv))
accuracy_sd = sd(as.numeric(cv))
# > accuracy
# [1] 0.9133333
# > accuracy_sd
# [1] 0.06324555

# Visualización del conjunto de entrenamiento
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


# Visualización del conjunto de testing
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