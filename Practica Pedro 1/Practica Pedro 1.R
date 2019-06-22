#Practica de Pedro 
datos = read.csv2("ADSL (3).csv")

#Tabla Atributos
x1=datos$provincia
FA1=table(x1)
FA1
(FR1=prop.table(FA1))
cbind (FA1,FR1)

#Tabla Discretas
x2=datos$CentrosSalud
FA2=table(x2)
FR2=round(prop.table(FA2),3)
FAA2=cumsum(FA2)
FRA2=cumsum(FR2)
cbind(FA2,FR2,FAA2,FRA2)

#Tabla Intervalos
x3=datos$ingresos
ext=c(500,1000,1500,2000,3000,4000,6000)
x3int=cut(x3,breaks=ext)
FA3=table(x3int)
FR3=round(prop.table(FA3),3)
FAA3=cumsum(FA3)
FRA3=cumsum(FR3)
cbind(FA3,FR3,FAA3,FRA3)

#Gráficas para Variables Discretas:
#-Diagrama de Barras
plot(FA2,type="h",col="purple")
#-Poligono de Frecuencia
lines(FA2,type="l",col="cyan")
#-Diagrama Acumulado
plot(FAA2,type="s")
#-Histograma
h=hist(x3,breaks=ext,col="dark orange")
h
plot(h$mids, h$density, type="l")
#-Poligono de Fecuencia Acumulada
plot(FAA3,type="l",main="estudio")


#Gráficas de Atributos
#-Diagrama de Rectangulos
barplot(FA1,col=rainbow(6))
#-Diagrama de Segmentos
pie(FA1,col=rainbow(8),main="Poblacion que duerme siesta")


#Medidas Estadísticas

#Media Aritmetica
mean(x3)
median(x3)
t=table(x3)
which(t==max(t))
Q1=quantile(x3,0.25,type=2)
Q3=quantile(x3,0.75,type=2)
Cuasivar=var(x3)

IQR(x3)
IQR3=Q3-Q1; IQR3

n=length(x3)
varianza=((n-1)/n)*Cuasivar
varianza

#Series Estadísticas de dos caracteres
#Tablas de Doble entrada
x4=datos$claspob
FA4=table(x1,x4)
prop.table(FA4)
FAM4=margin.table(prop.table(FA4),margin=2)
FAM4
