# Regresión Logística para predicción y clasificación.

#PASO 1. Importar el dataset
dataset = read.csv('Datasets/Social_Network_Ads.csv')
dataset = dataset[, 3:5] #Se seleccionan las 3 últimas columnas: Age, EstimatedSalary, Purchased

# Dividir los datos en conjunto de entrenamiento y conjunto de test
# install.packages("caTools")
library(caTools)
set.seed(129990)
split = sample.split(dataset$Purchased, SplitRatio = 0.75) #Se generan 300 observaciones para entrenar y las 100 restantes para validar.
training_set = subset(dataset, split == TRUE)
testing_set = subset(dataset, split == FALSE)

#PASO 2. Escalado de valores.
#En este caso sí es necesario escalar ya que los datos que se encuentran en al columna de edad son muy bajos con respecto a los que hay en la columna de EstimatedSalary. Es decir, ambas columnas tienen escalas diferentes y se hace necesario normalizar.
training_set[,1:2] = scale(training_set[,1:2])
testing_set[,1:2] = scale(testing_set[,1:2])

#Paso 3. Ajustar el modelo de regresión logística con el conjunto de entrenamiento.
# En este caso en lugar de crear una "regresión" se crea una "clasificación" la cual se puede obtener por medio de la función glm.
# La sintaxis de glm (Generalized Lineal Model) es muy similar a la de "lm" pero se debe especificar el parámetro "family" el cual por defecto es gausiano.
# En este caso, dado que se intenta predecir si la variable dependiente "Purchased" es 1 o 0, se utiliza la familia "binomial".
classifier = glm(formula = Purchased ~ .,
                 data = training_set, 
                 family = binomial)

#Paso 4. Predicción de los resultados con el conjunto de testing
# Se evalua con el modelo generado en el paso anterior los datos testing. 
# En este caso el algoritmo cambia un poco comparado con los vistos en sobre regresión ya que en este caso se predice una probabilidad entre 0 y 1.
# La función "predict" recibe el clasificador, tipo y datos de testing. 
# El clasificador representa el modelo generado con la función "glm" anteriormente.
# El tipo recibirá el parámetro "response" para que devuelva el conjunto de probabilidades para el dataset en un solo vector para posteriormente traducir a "venta" o "no venta".
# El newdata representa el conjunto de datos de testing (training_set). En este caso se elimina la columna "Purchased" ya que es precisamente la que se pretende predecir (variable dependiente) 
prob_pred = predict(classifier, type = "response",
                    newdata = testing_set[,-3]) # Ahora se puede verificar el valor de la variable "prob_pred" desde la terminal, de esta manera se puede comprobar que la predicción aporta datos estadísticos sobre cada uno de los registros del conjunto de pruebas. 
                                                # Con dicha información, se compara con el dataset de testing original para verificar la probabilidad y el dato real de "compra" o "no compra".


#Paso 5. Se utiliza la instrucción "ifelse" que trabajará sobre el vector de predicción.
# la condición concretamente es "prob_pred> 0.5" la cual si se cumple, devolverá 1 y sino 0. 
#Hay que tener en cuenta que devolverá un vector con un 0 o 1 para cada uno de los registros. 
y_pred = ifelse(prob_pred> 0.9, 1, 0)

#Paso 6. Crear la matriz de confusión.
#La matriz de confusión es un término que se utiliza para determinar el nivel de error hacia arriba o hacia abajo.
#La función table se encarga de comprobar los resultados de dos vectores.
#En este caso concreto interesa enviar el vector correspondiente a los valores de la columna 3 del conjunto de testing (Columna Purchased)
#Y el vector de de predicción generado en la línea anterior.
#La función table realiza una agrupación y suma los valores de 1 (compra) y 0 (no compra).
cm = table(testing_set[, 3], y_pred)
#Después de ejecutar la instrucción anterior se puede ver por la terminal:
# > cm = table(testing_set[, 3], y_pred)
# > cm
# y_pred
#    0  1
# 0 57  7
# 1 10 26

#Los resultados son sencillos de interpretar. 
# Significa que 57 realmente no han comprado y 26 sí lo han hecho.
# Es decir, que 57 registros del conjunto de testing se han predecido correctamente con un 0 y 26 se han predecido correctamente con 1. 
# Esto significa que el porcentaje de predicción correcto ha sido del 87% frente a un 13% incorrecto.

# Paso 7. Visualización del conjunto de entrenamiento.
# Con el siguiente código se generará una nube de puntos en donde se podrán ver las observaciones del conjunto de entrenamiento. Hay una linea que divide al conjunto rojo del verde, indicando cuales han "comprado" (1) y cuales no (0).
# Se pondrán ver algunas observaciones en verde que aparecen en el cuadrante rojo y viceversa, representan precisamente aquellas predicciones incorrectas que ha generado el modelo, es decir, aquellas que el modelo ha predicho que no ha comprado (0) pero en el dataset se indica que sí y viceversa. 
###111
set = training_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 0.01)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 0.01)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('Age', 'EstimatedSalary')
prob_set = predict(classifier, type = 'response', newdata = grid_set)
y_grid = ifelse(prob_set > 0.5, 1, 0)
plot(set[, -3],
     main = 'Clasificación (Conjunto de Entrenamiento)',
     xlab = 'Edad', ylab = 'Sueldo Estimado',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))


#Punto 7.1 Visualización del conjunto de testing
set = testing_set
X1 = seq(min(set[, 1]) - 1, max(set[, 1]) + 1, by = 10)
X2 = seq(min(set[, 2]) - 1, max(set[, 2]) + 1, by = 10000)
grid_set = expand.grid(X1, X2)
colnames(grid_set) = c('Age', 'EstimatedSalary')
prob_set = predict(classifier, type = 'response', newdata = grid_set)
y_grid = ifelse(prob_set > 0.5, 1, 0)
plot(set[, -3],
     main = 'Clasificación (Conjunto de Testing)',
     xlab = 'Edad', ylab = 'Sueldo Estimado',
     xlim = range(X1), ylim = range(X2))
contour(X1, X2, matrix(as.numeric(y_grid), length(X1), length(X2)), add = TRUE)
points(grid_set, pch = '.', col = ifelse(y_grid == 1, 'springgreen3', 'tomato'))
points(set, pch = 21, bg = ifelse(set[, 3] == 1, 'green4', 'red3'))