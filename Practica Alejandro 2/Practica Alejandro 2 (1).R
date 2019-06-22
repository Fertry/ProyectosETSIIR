#Leer los datos
datos1 = read.csv2("ADSL(1).csv")

#Extraer datos concretos. Asignarlos a variables
x1 = datos$provincia
x2 = datos$ingresos

#Frecuancia absoluta 2 variables
table(x1,x2)

#Creacion de un intervalo. Frec. absoluta con intervalo aplicado
ext = c(500,1000,1500,2000,6000)
intx2 = cut(x2,breaks = ext)
table (x1,intx2)

#Frec. absoluta de una variable
FA1 = table(x1)

#Conj es frec. absoluta de x1 y de x2 con intervalo aplicado
#conRel es frec. relativa de x1 y de x2 con intervalo aplicado
conj = table (x1,intx2)
conjRel = prop.table(conj)
conjRel

#Frecuencia absoluta marginal de X y de Y
marX = margin.table(conj,margin = 1)
marY = margin.table(conj,margin = 2)

#Frecuencia relativa marginal de X y de Y
marRelX = margin.table(prop.table(conj,margin = 1))
marRelY = margin.table(prop.table(conj,margin = 2))

#Frecuencia relativa condicionada a X
marRelCondX = prop.table(table(conj),1)

#Extraer datos de una matriz (conj es una "matriz") 
conj[2,3]

#Extraer datos concretos. Asignarlos a variables
x = datos$poblacion
y = datos$adsl

#Covarianza de x / y
cov(x,y)

#Dibujamos
plot(x,y,type = "p",main = "Gráfica",col = "green",xlab = "Poblacion",ylab = "ADSL")

#Dibujamos acotando
plot(x,y,type = "p",xlim = c(0,10000),ylim = c(0,13000),main = "Gráfica",col = "green",xlab = "Poblacion",ylab = "ADSL")

#Recta de Regresión
rYX = lm(y~x)
rYX

#Extraer datos concretos de la recta de regresión
a = rYX$coefficients[1]
b = rYX$coefficients[2]

#Estimaciones con la recta de regresión
predict(rYX)
predict(rYX,newdata = data.frame(x = c(240000,300000)))

#Dibujar la recta de regresión
abline(a,b,col = "red")
