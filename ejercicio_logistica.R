#Ejercicio:
# Utilizar el dataset titanic.csv y aplicar el modelo de regresión logistica.
# El modelo utilizará la columna dependiente "Survived" para predecir los supervivientes o no supervivientes.
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
