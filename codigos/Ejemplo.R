print("*********************************************");
print("* ANALISIS DE CALIFICACIONES");
print("* Por: Dr. Jorge Chávez Carlos.");
print("*********************************************");
#install.packages("readxl")
"library(readxl)
Lista = read.csv("https://raw.githubusercontent.com/NuclearGeorge/CURSO-DE-R/master/archivos/CALIFICACIONES.csv", header = FALSE)
x = Lista[[9]] # Calificaciones Totales
y = as.numeric(x)[12:length(x)]; # Números
l = length(y);
z = y[!is.na(y)];
prom = mean(z)
print(paste0("En el curso de Ecuaciones Diferenciales hay ", l," alumnos"))
print(paste0("El promedio de los estudiantes aprobados es: ", prom))
# Para las tareas
tareas = as.numeric(Lista[[6]])[12:l]*10/2
#Para los examenes
examenes = as.numeric(Lista[[7]])[12:l]*10/8
# Regresión Lineal
reg = lm(examenes~tareas)
ab = reg$coefficients
#summary(reg)
r2 = cor(examenes,tareas)^2
print(paste0("La correlación entre calificación de tareas y examenes es: ",r2))
#Gráfica de datos y regresion lineal
plot(tareas,examenes)
abline(reg, col = "red")
title(main = "CALIFICACIONES")
