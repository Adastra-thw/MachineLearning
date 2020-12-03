class Calculadora:

	def __init__(self):
		print("Hola desde el constructor")

	def suma(self, param1, param2):
		print(self.nombre)
		self.resultadoSuma = param1+param2 		
		return self.resultadoSuma		

	def resta(self, param1, param2):
		return param1-param2

	def multi(self, param1, param2):
		return param1*param2

	def divi(self, param1, param2):
		return param1/param2


cal = Calculadora()
print(cal.suma(10009, 2900))
print(cal.resultadoSuma)
