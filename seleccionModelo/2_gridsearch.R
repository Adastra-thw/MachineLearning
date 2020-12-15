# Grid Search

#Paso 1. Importar el dataset
dataset = read.csv('Datasets/Social_Network_Ads.csv')
dataset = dataset[, 3:5]
dataset$Purchased = factor(dataset$Purchased)

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
library(e1071)
classifier = svm(formula = Purchased ~ .,
                 data = training_set, 
                 type = "C-classification",
                 kernel = "radial")
# Predicción de los resultados con el conjunto de testing
y_pred = predict(classifier, newdata = testing_set[,-3])
# Crear la matriz de confusión
cm = table(testing_set[, 3], y_pred)

#Paso 5. Aplicar algoritmo de k-fold cross validation
# install.packages("caret")
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
accuracy = mean(as.numeric(cv))
accuracy_sd = sd(as.numeric(cv))

#Paso 6. Aplicar Grid Search para encontrar los parámetros óptimos.
#Se utiliza la función train para aplicar la técnica de Grid Search. Como ocurre con otros modelos vistos, es necesario indicar 
#la relación de variables independientes y dependientes, el dataset de entrenamiento y finalmente el modelo utilizado.
#Probablemente el parámetro más importante en este punto es precisamente el método, ya que representa una cadena con el modelo que utilizará caret.
#Los métodos disponibles se encuentran documentados en el github oficial de la librería caret.
install.packages("caret")
#install.packages("kernlab")
library(caret)
classifier = train(form = Purchased ~ .,
                   data = training_set, method = 'svmRadial')
classifier
classifier$bestTune

##Una vez ejecutada la función train con el modelo "SVM con kernel Radial" saldrán unos resultados similares a los siguientes:
# > classifier
# Support Vector Machines with Radial Basis Function Kernel 

# Tuning parameter 'sigma' was held constant at a value of 1.18122
# ...........
# Accuracy was used to select the optimal model using the largest value.
# The final values used for the model were sigma = 1.18122 and C = 1.
# > classifier$bestTune
# sigma C
# 3 1.18122 1

# Significa que los hiperparametros "sigma" y "C" dan mejores resultados con valores indicados.
# classifier = svm(formula = Purchased ~ .,
#                  data = training_fold, 
#                  type = "C-classification",
#                  kernel = "radial", C=1, sigma=1.18122)

