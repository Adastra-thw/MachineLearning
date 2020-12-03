class Calculadora:
	def __init__(self, parametro):
		self.parametro = parametro

	def suma(self, param1, param2, test="Pepe"):
		variable =  param1 + param2
		print(variable)
		return variable 

	def resta(self, param1, param2):
		return param1 - param2

	def multi(self, param1, param2):
		return param1 * param2

	def divi(self, param1, param2):
		res = 0
		if param2 == 0:
			print("Es cero")
		elif param2 > 0:
			res = param1 / param2
		else:
			print("Menor a cero")
		return res
	def __str__(self):
		return self.parametro


c1 = Calculadora("Cal1")
c2 = Calculadora("Cal2")
c3 = Calculadora("Cal3")

lst = [c1, c2, c3]

dicti = {"cal1": c1, "cal2": c2, "cal3": c3}


for key in dicti.keys():
	print(dicti[key])

for value in dicti.values():
	print(value)


for indice in lst:
	print(indice)


'''ret = suma(12, 121, test="Juan")
ret = divi(12,2)
print(ret)
'''


