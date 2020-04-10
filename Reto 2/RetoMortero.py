import matplotlib.pyplot as plt
from matplotlib.patches import Circle
import numpy as np
from mpl_toolkits import mplot3d
from Bezier import Bezier
from numpy import array as a
from mpl_toolkits.mplot3d import Axes3D
import mpl_toolkits.mplot3d.art3d as art3d

fig = plt.figure()
ax = fig.add_subplot(111, projection='3d')

h = -1 #Altura (z)
b = 0.999999999 #Radio 2
cg = 0
c = 0.55191502449 #constante para mostrar un circulo

#Vaso del mortero (cuerpo)
for i in range(0, 65):
	if i == 0:
		p = Circle((0, 0), b, color="blue", fill=True)
	else:
		p = Circle((0, 0), b, color="blue", fill=False)
	ax.add_patch(p)
	art3d.pathpatch_2d_to_3d(p, z = h)

	if i<10:
		b = b + 0.1
	elif i<15:
		b = b - 0.1
	elif i<30:
		b = b + 0.1
	elif i<55:
		b= b
	elif i>55:
		b = b -0.1

	h = h + 0.02

x = 0
points_set_1 = a([[-3, -b-1, h], [-1, -b- x, h], [1, -b - x, h], [3, -b-1, h]])
t_points = np.arange(0, 1, 0.01)
curve_set_1 = Bezier.Curve(t_points, points_set_1)
ax.plot(curve_set_1[:, 0], curve_set_1[:, 1], curve_set_1[:, 2], color="blue")
#Generar con curvas de Bezier la salida del mortero

ax.set_xlim(-5, 5)
ax.set_ylim(-5, 5)
ax.set_zlim(-2, 2)
plt.show()
