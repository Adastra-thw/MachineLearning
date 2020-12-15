#Ejercicio:
# Utilizar el dataset titanic.csv y aplicar el modelo de regresión logistica.
# El modelo utilizará la columna dependiente "" para predecir los supervivientes o no supervivientes.
# Visualizar los datos con las funciones head y str. Además determinar si hay cualquier dato NA.
# Partiendo de los datos anteriores, generar los siguientes gráficos (utilizar ggplot).
#   - Gráfico de barras con la variable "Survived"
#   - Gráfico de barras con la variable "Pclass"
#   - Gráfico de barras con la variable "Sex"
#   - Gráfico histograma con la variable "Age"
# Preprocesar datos: 
#   Cuál de las columnas del dataset tiene valores NA?
#   Detectar dichas columnas y sustituir valores NA con la media de valores de dicha columna.
#   Eliminar las columnas que no se utilizarán para el modelo logistico:
#       PassengerId, Name, Ticket, Cabin.
#   Categorizar con la función factor las columnas:
#        Survived, Pclass, Parch, SibSp.
# Dividir los datos en entrenamiento y prueba. 75% entrenamiento, 25% prueba.
#       La variable dependiente a predecir será Survived.
# Aplicar el modelo de regresión logistica sobre los datos de entrenamiento.
#     Aplicar la técnica de eliminación hacia atrás, eliminando una a una las variables menos significativas y analizando el modelo en cada pasada tras eliminar cada variable.
#     NO eliminar varias variables en cada pasada, hacerlo una a una y analizar el comportamiento del modelo. 
# Aplicar las predicciones contra el conjunto de pruebas y comparar con los datos reales.
#     Sobre las predicciones generadas, aplicar la instrucción ifelse y si el valor es mayor a 0,5, se debe asignar 1 (sobrevive) de lo contrario 0 (no sobrevive).
# Calcular la media de los resultados erroneos en la predicción.
#     mediaError = mean (resultados != testing_set$Survived)
#     Cuál es la precisión? considerando que es 1 - mediaError
dataset <- read.csv("DataSets/titanic.csv")
head(dataset)
any(is.na(dataset$Age))

library(ggplot2)
ggplot(dataset, aes(Survived)) + geom_bar()
ggplot(dataset, aes(Pclass)) + geom_bar()
ggplot(dataset, aes(Sex)) + geom_bar()
ggplot(dataset, aes(Age)) + geom_histogram()
ggplot(dataset, aes(Pclass, Age)) + geom_boxplot(aes(gropu=Pclass, fill=factor(Pclass)))

# Tratamiento de los valores NA
dataset$Age = ifelse(is.na(dataset$Age),
                     ave(dataset$Age, FUN = function(x) mean(x, na.rm = TRUE)),
                     dataset$Age)
any(is.na(dataset$Age))
head(dataset)
dataset <- dataset[-1]
dataset <- dataset[-3]
dataset <- dataset[-7]
dataset <- dataset[-8]


dataset$Survived = factor(dataset$Survived)
dataset$Pclass = factor(dataset$Pclass)
dataset$Parch = factor(dataset$Parch)
dataset$SibSp = factor(dataset$SibSp)


library(caTools)
set.seed(123)
split = sample.split(dataset$Survived, SplitRatio = 0.75)
training_set = subset(dataset, split == TRUE)
testing_set = subset(dataset, split == FALSE)

modelo <- glm(Survived  ~ . , family=binomial(link='logit'), data=training_set)
summary(modelo)

predicciones <- predict(modelo, testing_set, type='response')
resultados <- ifelse(predicciones > 0.5, 1, 0)
head(resultados)

error <- mean( resultados != testing_set$Survived )
print(error)
precision = 1 - error
print(precision)
