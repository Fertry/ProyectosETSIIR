## ------------------------------------------------------------------------
datos = read.csv2("ADSL.csv")
str(datos)
head(datos)

## ------------------------------------------------------------------------
x1 = datos$provincia
FrecAbs1 = table(x1)
FrecAbs1

FrecRel1 = FrecAbs1/length(x1)
FrecRel1

## ------------------------------------------------------------------------
FrecRel1 = prop.table(FrecAbs1)
FrecRel1

tabla1=cbind(FrecAbs1,FrecRel1)
tabla1

## ------------------------------------------------------------------------
x2 = datos$CentrosSalud
FrecAbs2 = table(x2)
FrecAbs2

FrecRel2 = prop.table(FrecAbs2)
FrecRel2

AbsAcum2 = cumsum(FrecAbs2)
AbsAcum2

RelAcum2 = cumsum(FrecRel2)
RelAcum2

tabla2=cbind(FrecAbs2,FrecRel2,AbsAcum2,RelAcum2)
tabla2

## ------------------------------------------------------------------------
x3 = datos$ingresos

extremos = c(500,1000,1500,2000,3000,4000,6000)

intervalos = cut(x3,breaks=extremos)
head(intervalos)

FrecAbs3 = table(intervalos)
FrecAbs3

FrecRel3 = prop.table(FrecAbs3)
FrecRel3

AbsAcum3 = cumsum(FrecAbs3)
AbsAcum3

RelAcum3 = cumsum(FrecRel3)
RelAcum3

tabla3=cbind(FrecAbs3,FrecRel3,AbsAcum3,RelAcum3)
tabla3

## ----fig.height=4,fig.width=4,out.width='10cm',fig.align='center'--------
barplot(FrecAbs1,main="Provincia")
barplot(FrecAbs1,horiz=TRUE,col=c("blue","yellow"),main="Provincia")     # Horizontal

## ----fig.height=4,fig.width=4,out.width='10cm',fig.align='center'--------
pie(FrecAbs1,main="Provincia")
pie(FrecAbs1,col=rainbow(8),main="Provincia")        # Paleta rainbow con 8 colores

## ----fig.height=4,fig.width=4,out.width='8.5cm',fig.align='center'-------
plot(FrecAbs2,type="h",xlab="Número de centros de salud",ylab="Frecuencia absoluta",lwd=5,
     main="Centros de salud según municipio",col="blue")

## ----fig.height=4,fig.width=4,out.width='8.5cm',fig.align='center'-------
plot(FrecAbs2,type="l",xlab="Número de centros de salud",ylab="Frecuencia absoluta",lwd=5,
     main="Centros de salud según municipio",col="blue")

## ----fig.height=4,fig.width=4,out.width='9cm',fig.align='center'---------
plot(FrecAbs2,type="h",xlab="Número de centros de salud",ylab="Frecuencia absoluta",lwd=5,
     lty=3,main="Centros de salud según municipio",col="blue")
lines(FrecAbs2,type="l",lwd=2,col="red")

## ----fig.height=6,fig.width=6,out.width='9cm',fig.align='center'---------

plot(as.table(AbsAcum2),type="s",xlab="Número de centros de salud",ylab="Frecuencia acumulada",
     lwd=2,main="Centros de salud según municipio (curva acumulativa)")

## ----fig.height=4,fig.width=4,out.width='8.5cm',fig.align='center'-------
hist(x3,breaks=extremos,main="Histograma",col="lightblue",xlab="Ingresos por habitante",
     ylab="Densidad relativa")

## ----fig.height=4,fig.width=4,out.width='8.5cm',fig.align='center'-------
h = hist(x3,breaks=extremos,main="Histograma y polígono de frecuencias",col="lightblue",
         xlab="Ingresos por habitante",ylab="Densidad relativa")
lines(h$mids,h$density,type="l",col="red",lwd=2)

## ----fig.height=4,fig.width=4,out.width='9cm',fig.align='center'---------
plot(extremos,c(0,AbsAcum3),type="l",lwd=2,main="Curva Acumulativa",
     xlab="Ingresos por habitante",ylab="Frecuencia acumulada")

## ----fig.height=4,fig.width=4,out.width='9cm',fig.align='center'---------
vx=c(0,extremos,10000)
vy=c(0,0,AbsAcum3,length(x3))    
plot(vx,vy,type="l",lwd=2,main="Curva Acumulativa",xlab="Ingresos por habitante",
     ylab="Frecuencia acumulada")

## ------------------------------------------------------------------------
x4 = datos$rdsi

## ------------------------------------------------------------------------
mean(x4)

## ------------------------------------------------------------------------
median(x4)

## ------------------------------------------------------------------------
t = table(x4)
which(t == max(t))

## ------------------------------------------------------------------------
Q = quantile(x4,c(0.25,0.5,0.75), type=2)
Q

## ------------------------------------------------------------------------
Rango = max(x4)-min(x4)
Rango

## ------------------------------------------------------------------------
rIQR = Q[3]-Q[1]
as.numeric(rIQR)

## ------------------------------------------------------------------------
Cuasivar = var(x4)
Cuasivar

## ------------------------------------------------------------------------
sd(x4)

## ------------------------------------------------------------------------
n = length(x4)
varianza = Cuasivar*((n-1)/n)
varianza

## ------------------------------------------------------------------------
destip = sqrt(varianza)
destip

## ------------------------------------------------------------------------
resumen = summary(x4)
resumen

