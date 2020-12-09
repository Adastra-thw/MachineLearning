#Regresión con árbol de decisión

# Paso 1. Importar el dataset
dataset = read.csv('Datasets/Position_Salaries.csv')
dataset = dataset[, 2:3]

# Paso 2. Se debe instalar la librería "rpart" la cual permite utilizar árboles de decisión para regresión.
#La librería cuenta con la función "rpart" que permite utilizar los parámetros formula y dataset, que son comunes en la función "lm" vista en los modelos anteriores.
#Además de dichos parámetros, también se cuenta con el parámetro "control" el cual como su nombre indica, permite controlar el comportamiento del algoritmo de árbol de decisión.
#El control que se permite aplicar incluye, entre otras cosas, el número mínimo o máximo de observaciones que deben existir en una hoja entre otras cosas. 
#install.packages("rpart")
library(rpart)
regression = rpart(formula = Salary ~ .,
                   data = dataset,
                   control = rpart.control(minsplit = 2))

# Paso 3. Predicción de nuevos resultados con Árbol Regresión. 
# Se puede cambiar varias veces el "Level" y se podrá ver que la predicción generada no es tan buena como se espera. Esto se debe a que este modelo no funciona del todo bien con el conjunto de datos suministrado.
y_pred = predict(regression, newdata = data.frame(Level = 6.5))


#Paso 4. Se genera la gráfica para comprobar el comportamiento del módelo con las predicciones y los diferentes puntos de observación que se encuentran en el dataset original.
# Visualización del modelo de árbol de regresión
# install.packages("ggplot2")
# Visualización del modelo de árbol de regresión
# install.packages("ggplot2")
library(ggplot2)
x_grid = seq(min(dataset$Level), max(dataset$Level), 0.1)
ggplot() +
  geom_point(aes(x = dataset$Level , y = dataset$Salary),
             color = "red") +
  geom_line(aes(x = dataset$Level, y = predict(regression, 
                                        newdata = data.frame(Level = dataset$Level))),
            color = "blue") +
  ggtitle("Predicción con Árbol de Decisión (Modelo de Regresión)") +
  xlab("Nivel del empleado") +
  ylab("Sueldo (en $)")