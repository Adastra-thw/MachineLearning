#Regresión con máquinas de soporte vectorial.

#Paso 1. Importar el dataset.
dataset = read.csv('Datasets/Position_Salaries.csv')
dataset = dataset[, 2:3]


#Paso 2. Utilizar SVR con el conjunto de datos. Se debe importar la librería e1071.
#En este caso es conveniente ver la documentación de la función svm: ?svm
#El tipo en este caso será eps-regression tal como se ha visto en la documentación de la función.
#install.packages("e1071")
library(e1071)
regression = svm(formula = Salary ~ ., 
                 data = dataset, 
                 type = "eps-regression", 
                 kernel = "radial")#radial es el valor por defecto y funciona bien para el entranmiento y predicción de datos que no siguen un comportamiento lineal.

#Paso 3. Generar la predicción de nuevos resultados con el SVM. El resultado indica que el modelo SVM es valido ya que para el Level indicado ha producido valor que está el rango esperado entre los valores 6 y 7 de la columna "Level" del dataset.
y_pred = predict(regression, newdata = data.frame(Level = 6.5))

#Paso 4. Visualización del modelo de SVR.
#El modelo se adapta muy bien, la mayoría de los puntos de observación se encuentran bastante cerca de la curva trazada.
#Se puede observar que el modelo funciona para casi todos pero hay puntos de observación que no se gestionan adecuadamente ya que son irregulares o atipicos, los cuales son menospreciados por el modelo ya que precisamente, lo que intenta es definir vectores de soporte que cubran la mayor cantidad de puntos de observación, pero en ocasiones no pueden cubrirlos todos.
# install.packages("ggplot2")
library(ggplot2)
x_grid = seq(min(dataset$Level), max(dataset$Level), 0.1)
ggplot() +
  geom_point(aes(x = dataset$Level , y = dataset$Salary),
             color = "red") +
  geom_line(aes(x = dataset$Level, y = predict(regression, 
                                               newdata = data.frame(Level = dataset$Level))),
            color = "blue") +
  ggtitle("Predicción (SVR)") +
  xlab("Nivel del empleado") +
  ylab("Sueldo (en $)")

