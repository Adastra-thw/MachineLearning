class Persona:
		
	def __init__(self, nombre, apellidos, edad):
		self.nombre = nombre
		self.apellidos = apellidos
		self.edad = edad

	def dormir(self, horas):
		print("He dormido: "+str(horas))

	def __str__(self):
		return "Enseño esto"


class Estudiante(Persona):
	def __init__(self):
		Persona.__init__(self, "juan", "lopez", 34)	
		self.variable = 10
	
	def estudiar(self):
		print("Estudiando")
	
	
	def dormir(self, horas):
		if horas >= 5:
			print("suficiente")
		else:
			print("muy poco")
		


class EstudianteMaster(Estudiante):
	def metodoTest(self):
		print("Prueba")
		print(self.variable)
master.nombre = "Pepe"
master.metodoTest()
print(master.nombre)
print(master.edad)
print(master.apellidos)
print(master.variable)

#x = Estudiante("Juan", "Lopez", 22)
#x.dormir(8)

#y = Persona("ana","lopez",33)
#y.dormir(9)


#z = Persona("david","suarez",33)


