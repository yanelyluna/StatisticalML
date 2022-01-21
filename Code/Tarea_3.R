library(MASS)
library(boot)
#Ejercicio 8
#Creamos las simulaciones
set.seed(1)
y=rnorm(100) #Para crear el vector
x=rnorm(100)
y=x-2*x^2+rnorm (100)
#Tenemos n=100, p=2 y el modelo es
#$$Y=x-2X^2+error$$
#Gráficamos nuestros datos
plot(x, y, main = "Scatterplot",pch = 18, col = 3)
#Podemos observar que tiene una curva debido a la forma como fue 
#construida
#Creamos una variable donde guardaremos los datos
datos<-cbind(y,x)
datos<-as.data.frame(datos)
#Ajustamos los modelos de regresion
modelos<-list()
for(i in 1:4){
  modelos[[i]]<-glm(y~poly(x,i),data = datos)
}

#Calculamos los errores con el método de CV con k=20 y B=500
B500_102<-matrix(NA,4,2)
B=500
for(i in 1:4){
  set.seed(102)
  err.cv=t(replicate(B, (cv.glm(datos,modelos[[i]],K=20))$delta))
  B500_102[i,]<- (bf.err.cv5.avB1=apply(err.cv, 2, mean))
}
B500_102

#Calculamos los errores de predicción usando CV con k=20 y B=1000
B1000_102<-matrix(NA,4,2)
B=1000
for(i in 1:4){
  set.seed(102)
  err.cv=t(replicate(B, (cv.glm(datos,modelos[[i]],K=20))$delta))
  B1000_102[i,]<- (bf.err.cv5.avB1=apply(err.cv, 2, mean))
}
B1000_102
errores_102<-round(cbind(B500_102[,1],B1000_102[,1]),2)
errores_102


#Con una nueva semilla calculamos los errores de predicción usando CV con k=20 y B=500
B500_12<-matrix(NA,4,2)
B=500
for(i in 1:4){
  set.seed(12)
  err.cv=t(replicate(B, (cv.glm(datos,modelos[[i]],K=20))$delta))
  B500_12[i,]<- (bf.err.cv5.avB1=apply(err.cv, 2, mean))
}
B500_12

#Calculamos los errores con k=20 y B=1000
B1000_12<-matrix(NA,4,2)
B=1000
for(i in 1:4){
  set.seed(12)
  err.cv=t(replicate(B, (cv.glm(datos,modelos[[i]],K=20))$delta))
  B1000_12[i,]<- (bf.err.cv5.avB1=apply(err.cv, 2, mean))
}
B1000_12

############
#Entre B = 500 y B = 50000 Sólo varió en 2 décimas, por esto decidimos
#Usar 500 al ser máss óptimo en tiempo
errores_12<-round(cbind(B500_12[,1],B1000_12[,1]),2)
errores_12
errores<-cbind(errores_102,errores_12)
###############################################################
#e)
#El modelo 2 es el que tiene el menor error, esto ya lo esperabámos
#dado que nuestras observaciones tienen forma de una expresión 
#cuadrática.

summary(modelos[[1]])
summary(modelos[[2]])
summary(modelos[[3]])
summary(modelos[[4]])

#Podemos notar que el término cuadrático es altamente significativo
# No sucede esto mismo con el resto de los términos, esto tiene 
# sentido con los resultados de la validación cruzada pues, 
# al agregarle más variebles sin significancia estadística sólo 
#se incrementó el error del modelo.

#Ejercicio 9
attach(Boston)
#a)  Sacamos un estimador de la media muestral de la población
mu <- mean(medv); mu
#b)  Sacamos un estimador del error estándar de la media
#Para esto sacamos la desviación estándar muestral y la dividimos 
#entre la raiz cuadrada del número de observaciones
sd<-sd(medv); sd
n<-nrow(Boston)
se<-sd/sqrt(n); se #0.408861
#c)  Estimamos el error estándar usando la librería "boot"
set.seed(546) #fijamos la semilla
##1,000 iteracciones se=0.394782
mean.boot1000=boot(medv, function(x,i) mean(x[i]), R=1000); mean.boot1000
#Sacamos el error estándar del output
se.b1<-sd(mean.boot1000$t)
##10,000 iteracciones se=0.4104387
mean.boot10000=boot(medv, function(x,i) mean(x[i]), R=10000); mean.boot10000
se.b2<-sd(mean.boot10000$t)
##100,000 iteracciones se=0.4091673
mean.boot10e5=boot(medv, function(x,i) mean(x[i]), R=100000); mean.boot10e5
se.b3<-sd(mean.boot10e5$t)
##100,000 iteracciones se=0.4083909
######mean.boot10e6=boot(medv, function(x,i) mean(x[i]), R=1000000); mean.boot10e6
se.b4<-sd(mean.boot10e6$t)
#Observamos algunas simulaciones
plot(mean.boot1000)
plot(mean.boot10e5)
plot(mean.boot10e6)
#d) Encontramos un intervalo al 95% de confianza
ci.mean1<-c(mu-2*se.b1,mu+2*se.b1);ci.mean1
ci.mean2<-c(mu-2*se.b2,mu+2*se.b2);ci.mean2
ci.mean3<-c(mu-2*se.b3,mu+2*se.b3);ci.mean3
ci.mean4<-c(mu-2*se.b4,mu+2*se.b4);ci.mean4
#Comparamos con t test
t.test(medv)
#Notamos que es el mismo intervalo para cualquier estimación
#e)  Estimamos la mediana muestral
mdian<-median(medv); mdian 
#f)  Usando bootstrap con 1,000 iteraciones
mdian.boot1m <- boot(medv,function(x,i) median(x[i]),R=1000);mean(mdian.boot1m$t)
#Usando bootstrap con 10,000 iteraciones
mdian.boot10m <- boot(medv,function(x,i) median(x[i]),R=10000);mean(mdian.boot10m$t)
#Usando bootstrap con 100,000 iteraciones
mdian.boot100m <- boot(medv,function(x,i) median(x[i]),R=100000);mean(mdian.boot100m$t)
##Comparando los valores estimados
cbind(mean(mdian.boot1m$t),mean(mdian.boot10m$t),mean(mdian.boot100m$t))
cbind(sd(mdian.boot1m$t),sd(mdian.boot10m$t),sd(mdian.boot100m$t))
#Notamos que en en las tres estimaciones obtenermos valores entre 21.18 y 21.21
#de la mediana, mientras que el error estándar se encuentra
#entre 0.35 y 0.38, siendo el de la estimación con R=1000 el más pequeño.

#g)  Calculamos el percentil al 0.1 de medv
q10 <- quantile(medv,0.1); q10 
#h)  Usando bootstrap estiamamos el error estándar de q10r. Con R=1,000
q10.boot1m <- boot(medv,function(x,i) quantile(x[i],0.1),R=1000);mean(q10.boot1m$t)
#Usando bootstrap estiamamos el error estándar de q10r. Con R=10,000
q10.boot10m <- boot(medv,function(x,i) quantile(x[i],0.1),R=10000);mean(q10.boot10m$t)
#Usando bootstrap estiamamos el error estándar de q10r. Con R=100,000
q10.boot100m <- boot(medv,function(x,i) quantile(x[i],0.1),R=100000);mean(q10.boot100m$t)
#Comparando los valores estimados
cbind(mean(q10.boot1m$t),mean(q10.boot10m$t),mean(q10.boot100m$t))
cbind(sd(q10.boot1m$t),sd(q10.boot10m$t),sd(q10.boot100m$t))
