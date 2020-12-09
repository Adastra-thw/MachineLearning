# Regresión lineal múltiple.
# Importar el dataset
dataset = read.csv('Datasets/50_Startups.csv')
#dataset = dataset[, 2:3]

#Categorizar las variables no númericas del modelo de datos.
dataset$State = factor(dataset$State, 
                       levels = c("New York", "California", "Florida"), 
                       labels = c(1,2,3))

# Dividir los datos en conjunto de entrenamiento y conjunto de test
#install.packages("caTools")
library(caTools)
set.seed(123)

split = sample.split(dataset$Profit, SplitRatio = 0.8) #80% de la muestra a predecir (variable dependiente)
entrenamiento = subset(dataset, split == TRUE) # Se genera el conjunto de entrenamiento.
pruebas = subset(dataset, split == FALSE) # Se genera el conjunto de pruebas.

#regresion = lm(formula = Profit ~ R.D.Spend + Administration + Marketing.Spend + State, data = entrenamiento) #la función sirve para Regresión lineal simple y múltiple.
#Con esta sintaxis, es equivalente a decir que la variable dependiente es "Profit" y las variables independientes son todas las demás. Igual la instrucción anterior.
regresion = lm(formula = Profit ~ ., data = entrenamiento) #la función sirve para Regresión lineal simple y múltiple.
summary(regresion)

#Predecir los resultados con los datos de prueba.
prediccion = predict(regresion, newdata = pruebas)

#Se enseñará información sobre los valores que se han predecido para cada uno 
#de los registros de pruebas. Pueden estar más o menos alejados del valor Profit, 
#pero se entiende que es un buen modelo ya que las diferencias no son tan amplias.
print(prediccion)
#prediccion
#2         4         5         8        11        16        20        21 
#37766.77  44322.33  46195.35  55560.43  62115.99  71481.07  81782.66  89274.72 
#24        26 
#102385.84 109877.90 

#Se han utilizado todas las variables, pero se puede probar la regresión utilizando
#otras variables independientes que puedan ser mejor predictoras en función a su coeficiente. 


#Regresión líneal múltiple: ELIMINACIÓN HACIA ATRAS.
#Algunas variables pueden tener mayor impacto que otras, en la eliminación hacia atras
# se empieza como se ha visto anteriormente, seleccionando todas las variables independientes del modelo
# y a continuación, se eliminan las variables que son estadisticamente menos significativas.

#Construir un modelo óptimo con la eliminación hacia atras.
#Paso 1: Seleccionar todas las variables dependientes y aplicar el modelo de regresión sobre TODO el dataset, no solo el conjunto de entrenamiento.
regresion_hacia_atras = lm(formula = Profit ~ R.D.Spend + Administration + Marketing.Spend + State, data = dataset) 

#Si el P-Valor de una variable es superior al nivel de significancia (que tipicamente es 0,05), dicha variable no es relevante desde el punto de vista estadístico y se puede eliminar.
summary(regresion_hacia_atras)

#En el resultado que se aprecia, se puede ver la tabla con los coeficientes de cada variable. Se puede apreciar que las variables dummy "State2" y "State3" tienen el coeficiente más alto, por lo tanto es la menos relevante.
#Paso 2: Se elimina la variable independiente State ya que es la que menos relevancia estadística tiene.
regresion_hacia_atras = lm(formula = Profit ~ R.D.Spend + Administration + Marketing.Spend, data = dataset) 
summary(regresion_hacia_atras)

#Paso 3: Se eliminan las variables independientes Administration y Marketing.Spend ya que son las que menos relevancia estadística tienen.
regresion_hacia_atras = lm(formula = Profit ~ R.D.Spend, data = dataset) 
summary(regresion_hacia_atras)


#AUTOMATIZANDO TODO EL PROCESO

# backwardElimination <- function(x, sl) {
#   numVars = length(x)
#   for (i in c(1:numVars)){
#     regressor = lm(formula = Profit ~ ., data = x)
#     maxVar = max(coef(summary(regressor))[c(2:numVars), "Pr(>|t|)"])
#     if (maxVar > sl){
#       j = which(coef(summary(regressor))[c(2:numVars), "Pr(>|t|)"] == maxVar)
#       x = x[, -j]
#     }
#     numVars = numVars - 1
#   }
#   return(summary(regressor))
# }
# 
# SL = 0.05
# dataset = dataset[, c(1,2,3,4)]
# backwardElimination(pruebas, SL)