#Leer los datos
datos2 = read.csv2("ParoRegistrado(1).csv")

#Definir esta tabla como una serie temporal
serieTemporal = ts(data = datos2$Valor,frequency = 12,start = c(datos2$Anno[1],datos2$Mes[1]))
serieTemporal

#Descomponer la serie temporal
comp = decompose(serieTemporal,type = "multiplicative")
comp

#Extracción de la media movil
MM = comp$trend
MM

#Extracción del Indice de Variacion Estacional
IVE = comp$seasonal
IVE

#Dibujamos la serie temporal y las medias moviles por encima con "lines"
plot(serieTemporal,type = "l",col = "green")
lines(MM,type = "l",col = "red")

#Calculo de la serie desestacionalizada
serieDes = serieTemporal / IVE
serieDes

#Calculo de los tiempos que han medido la serie
tiempo = time(serieDes)
tiempo

#Calculo de la tendencia secular
rTendencia = lm(serieDes~tiempo)
rTendencia

#Extraer datos de la recta de tendencia
a = rTendencia$coefficients[1]
b = rTendencia$coefficients[2]

#Dibujarlo sobre la serie temporal
abline(a,b,col = "dark orange")

#Predicciones 
t0 = 2002 + (9-1) / 12
predict(rTendencia,newdata = data.frame(x = c(t0)))
