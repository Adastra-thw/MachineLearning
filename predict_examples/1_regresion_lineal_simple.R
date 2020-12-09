# Regresión lineal simple.
#Paso 1. Importar el dataset
dataset = read.csv('Datasets/Salary_Data.csv')
#dataset = dataset[, 2:3]

# Dividir los datos en conjunto de entrenamiento y conjunto de test
#install.packages("caTools")
library(caTools)
set.seed(123)
split = sample.split(dataset$Salary, SplitRatio = 2/3) #2 de cada 3 individuos de la variable a predecir (variable dependiente)
entrenamiento = subset(dataset, split == TRUE) # Se genera el conjunto de entrenamiento.
pruebas = subset(dataset, split == FALSE) # Se genera el conjunto de pruebas.

# Escalado de valores
# training_set[,2:3] = scale(training_set[,2:3])
# testing_set[,2:3] = scale(testing_set[,2:3])

# Modelo de regresión lineal simple con el set de training.
#En R la función "lm" permite crear un modelo lineal. ?lm
#El atributo "formula" permite indicar la relación entre la variable dependiente y la variable independiente.
#Su uso es el siguiente:
#lm(formula = VariableDependiente  ~ VariableIndependiente, data = DataSetR)
#En donde "DataSetR" será tipicamente el conjunto de datos para entrenamiento.
regresion = lm(formula = Salary ~ YearsExperience, data = entrenamiento)
summary(regresion)

#Partiendo del modelo anterior se procede a generar una predicción.
prediccion = predict(regresion, newdata = pruebas)
# print(prediccion) # Se debe comparar el resultado de la predicción con el dataset de pruebas.
# En general, cuantos más años de experiencia, el salario debe tener una tendencia creciente lineal.

#install.packages("ggplot2") #Instalar la librería de gráficos en R.
library(ggplot2)
#Partiendo de la predicción anterior, se procede a generar la gráfica con los datos de entrenamiento.
ggplot() +
   geom_point(aes(x = entrenamiento$YearsExperience, y = entrenamiento$Salary),
              colour = "red") + # Se definen los puntos "aes" representa cómo visualizar un un punto.
   geom_line(aes(x = pruebas$YearsExperience, 
                 y= prediccion), colour="blue") + #La línea se debe trazar en función a la predicción realizada en el modelo lineal utilizando los datos de entrenamiento.
   ggtitle("Relación del sueldo / años de experiencia (entrenamiento)") +
   xlab("Años de experiencia: ") +
   ylab("Sueldo: ")
