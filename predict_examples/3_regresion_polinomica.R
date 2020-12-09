# Regresión Polinómica

#Paso 1. Importar el dataset
dataset = read.csv('Datasets/Salaries.csv')
# Solamente se eleccionan las columnas 2 y 3 del dataset, que son los que interesan realmente. (Level, Salary)
dataset = dataset[, 2:3]

#Paso 3. Se ejecuta una regresión líneal con todas las variables para ver la nube de puntos y ver que un modelo lineal no es el más óptimo.
#El conjunto de datos en este caso es muy pequeño, por lo tanto no es práctico dividir el dataset en testing y training ya que podría afectar a los resultados.
#
lin_reg = lm(formula = Salary ~ ., 
             data = dataset)
#Para ver por qué el modelo líneal no es buena idea se procede a ver los coeficientes generados en la regresión. 
summary(lin_reg)
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)   
# (Intercept)  -195333     124790  -1.565  0.15615   
# Level          80879      20112   4.021  0.00383 **

# Se puede ver que el valor estimado es negativo, significa que el primer nivel empezaría no solo por debajo de lo que se indica en el dataset original sino que además, el modelo indica que alguien que acaba de empezar a trabajar "debe pagar" por ese trabajo.
#También se puede apreciar que a cada subida de nivel se incrementa 80879, lo cual tampoco encaja con el dataset original.
#Aunque la variable Level tiene un p valor óptimo que está por debajo del 0.05 (es un buen predictor) los datos arrojados indican que no es el modelo adecuado.

#Paso 4. Se ajusta el modelo de regresión Polinómica con el conjunto de datos para ver las diferencias.
#Un modelo polinómico es similar al lineal pero es necesario tratar las variables independentes y aplicar el cuadrado, el cubo o cualquier otra potencia a dichas variables. Esta es precisamente la diferencia entre un modelo líneal y el polinomico.
dataset$Level2 = dataset$Level^2
dataset$Level3 = dataset$Level^3
dataset$Level4 = dataset$Level^4
poly_reg = lm(formula = Salary ~ .,
              data = dataset)

#Paso 5. Se pueden observar los coeficientes y aunque las variables añadidas siguen teniendo algunas discrepacias el modelo refleja mejor la realidad.
summary(poly_reg)
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)   
# (Intercept)  184166.7    67768.0   2.718  0.04189 * 
#   Level       -211002.3    76382.2  -2.762  0.03972 * 
#   Level2        94765.4    26454.2   3.582  0.01584 * 
#   Level3       -15463.3     3535.0  -4.374  0.00719 **
#   Level4          890.2      159.8   5.570  0.00257 **

#Paso 6. Installar ggplot2 para los graficos. 
# install.packages("ggplot2")
library(ggplot2)

#Paso 6. Se procede a crear el gráfico para enseñar el modelo lineal y ver que no es el más apropiado.
ggplot() +
  geom_point(aes(x = dataset$Level , y = dataset$Salary),
             color = "red") +
  geom_line(aes(x = dataset$Level, y = predict(lin_reg, newdata = dataset)),
            color = "blue") +
  ggtitle("Predicción lineal del sueldo en función del nivel del empleado") +
  xlab("Nivel del empleado") +
  ylab("Sueldo")


#Paso 7. Se procede a crear el gráfico para enseñar el modelo polinómico y ver que es el más apropiado.
ggplot() +
  geom_point(aes(x = dataset$Level , y = dataset$Salary),
             color = "red") +
  geom_line(aes(x = dataset$Level, y = predict(poly_reg, newdata = dataset)),
            color = "blue") +
  ggtitle("Predicción polinómica del sueldo en función del nivel del empleado") +
  xlab("Nivel del empleado") +
  ylab("Sueldo")


#Paso 8. Visualización del modelo polinómico de una forma más "agradable" y sin tantos picos.
x_grid = seq(min(dataset$Level), max(dataset$Level), 0.1)
ggplot() +
  geom_point(aes(x = dataset$Level , y = dataset$Salary),
             color = "red") +
  geom_line(aes(x = x_grid, y = predict(poly_reg, 
                                        newdata = data.frame(Level = x_grid,
                                                             Level2 = x_grid^2,
                                                             Level3 = x_grid^3,
                                                             Level4 = x_grid^4))),
            color = "blue") +
  ggtitle("Predicción polinómica del suedlo en función del nivel del empleado") +
  xlab("Nivel del empleado") +
  ylab("Sueldo (en $)")

#Paso 9. Predicción de nuevos resultados con Regresión Lineal. 
# Para ello se debe usar el objeto de regresión lineal con la función predict. El atributo "newdata" recibira en este caso, la información del data frame cuyo nivel sea "6.5". 
# Con esto lo que intenta es predecir cuál debe ser el valor de X (la variable dependiente, que en este caso es Salary) cuando el nivel es 6.5 (columna Level)
y_pred = predict(lin_reg, newdata = data.frame(Level = 6.5))
#Tras ejecutar lo anterior, la predicción es de 330379, un valor que está muy por encima de la información real debido a que se está usando un modelo lineal.

#Paso 10. Predicción de nuevos resultados con Regresión Polinómica. En este caso se utiliza la regresión polinómica y las columnas de niveles para el valor 6.5. Notar que se ha tenido que ir potenciando cada columna nueva.
#Se puede apreciar que la predicción está mucho más adaptada y cercana a la información que se tiene.
y_pred_poly = predict(poly_reg, newdata = data.frame(Level = 6.5,
                                                     Level2 = 6.5^2,
                                                     Level3 = 6.5^3,
                                                     Level4 = 6.5^4))

