datos = read.csv2("ADSL.csv")

x1 = datos$provincia
FA1 = table(x1)
FA1
(FR1 = prop.table(FA1))
cbind(FA1)
cbind(FA1,FR1)

#comentario

x2 = datos$CentrosSalud
FA2 = table(x2)
FR2 = prop.table(FA2)
cbind(FA2,FR2)
cumsum(FA2)
FAA2 = cumsum(FA2)
FRA2 = cumsum(FR2)
cbind(FA2,FR2,FAA2,FRA2)

#comentario

x3 = datos$ingresos
ext = c(500,1000,1500,2000,3000,4000,6000)
x3int = cut(x3,breaks = ext)
FA3 = table(x3int)
FR3 = prop.table(FA3)
FAA3 = cumsum(FA3)
FRA3 = cumsum(FR3)
cbind(FA3,FR3,FAA3,FRA3)

#comentario

plot(FA2,type = "h")
plot(FA2,type = "h",col = "green")
plot(FR2,type = "h",col = "red")
plot(FR2,type = "l",col = "yellow")
lines(FA2,type = "l",col = "blue")

#comentario

hist(x3,breaks = ext,col = "green")
h = hist(x3,breaks = ext,col = "green") 
h
plot(h$mids,h$density,type = "l",col = "blue")

#comentario

plot(FAA3,type = "l")

#Comentario

barplot(FA1,col = "green")
barplot(FA1,col = rainbow(6))

pie(FA1,col = rainbow(8))

#Comentario

mean(x3)
mdian(x3)
median(x3)
t = table(x3)
which(t == max(t))
q1 = quantile(x3,0.25,type = 2)
q1
q3 = quantile(x3,0.75,type = 2)
q3
Rango = max(x3) - min(x3)
Rango
IQR(x3)
Iqr = q3 - q1
Iqr
Cuasivar = var(x3)
Cuasivar
sd(x3)
n = length(x3)
n
varianza = ((n-1)/n)*Cuasivar
varianza
destip = sqrt(varianza)
destip

#comentario

x4 = datos$claspob
FA4 = table(x1,x4)
FA4
FR4 = prop.table(FA4)
FR4
FAM4 = margin.table(FA4,margin = 2)
FAM4
