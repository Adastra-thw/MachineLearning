#Importar el dataset.

dataset = read.csv('/home/adastra/Escritorio/CursoR/Datasets/Data.csv')

#Gestión de datos faltantes.

dataset$Age = ifelse(is.na(dataset$Age), ave(dataset$Age, FUN = function(x) mean(x, na.rm = TRUE)), dataset$Age)
dataset$Salary = ifelse(is.na(dataset$Salary), ave(dataset$Salary, FUN = function(x) mean(x, na.rm = TRUE)), dataset$Salary)

View(dataset)

#Codificar las variables categóricas.
#Para aplicar modelos matemáticos (como regresiones lineales o logisticas) es necesario representar los datos no númericos como las cadenas 
#a una representación númerica que pueda ser aplicada en un formulas. Se crea un factor ordinal con "levels" y "labels".
dataset$Country = factor(dataset$Country, levels = c("Spain", "Germany", "France"), labels = c(1,2,3))

dataset$Purchased = factor(dataset$Purchased, levels = c("No", "Yes"), labels = c(0,1))

View(dataset)

#Dividir los datos em conjunto de entrenamiento y conjunto de test.
# Los datos para la fase de entrenamiento (entre un 70 y 80 porciento del dataset) se utilizará para enseñar al algoritmo cómo de debe predecir las variables dependientes partiendo de las variables dependientes.
# Los datos para la fase de test (entre un 20 y 30 porciento) se utilizarán para evaluar que el modelo funciona y la etapa de entrenamiento se ha llevado a cabo correctamente.

#install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(dataset$Purchased, SplitRatio = 0.8)
training = subset(dataset, split == TRUE)
testing =  subset(dataset, split == FALSE)


#En un modelo de datos puede ocurrir que la diferencia entre dos variables desde un punto de vista numerico no permita definir las diferencias propias de cada variable. 
# Por ejemplo, en el DataSet anterior, las variables edad y el salario tienen rangos diferentes y por lo tanto es importante definir el peso semantico de dichas variables, independientemente de su valor númerico.
# En este sentido hay que estandarizar los datos.
# El proceso de estandarizar consiste en la generación de una campana de Gauss para definir los valores normales y desviaciones.
# 
#Escalado de valores

#ERROR: La funcion "scale" se encarga de generar una media de los valores almacenados en cada columna. 
# No obstante, dichos valores deben ser numericos. En este caso no lo son, aunque al visualizar el Dataset, la columna "country" aparezca como un número, se trata de un factor, el cual se encuentra representado por una cadena. 
training = scale(training)
#ERROR: Lo mismo que lo anterior
testing = scale(testing)

#La solución consiste en escalar sólo aquellas columnas que tienen valores númericos.
training[,2:3] = scale(training[,2:3])
testing[,2:3] = scale(testing[,2:3])
