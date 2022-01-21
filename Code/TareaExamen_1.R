##TAREA 1 SML REGRESIÓN LINEAL
library(ISLR)
library(MASS)
library(corrplot)
library(broom)
library(ggplot2)
library(dplyr)
library(knitr)
library(xtable)
options(digits = 3)
#1)EJERCICIO 9###### 
data(Auto)
## Exploración de la base
str(Auto)
for(i in 1:9){print(anyNA(Auto[,i]))}
Auto$cylinders <- as.factor(Auto$cylinders)
Auto$origin <- as.factor(Auto$origin)
str(Auto)
names <- names(Auto)
#Histogramas de las variables predictoras
par(mfrow = c(2,3))
for(i in c(1,3,4,5,6,7)){hist(Auto[,i], main = names[i], xlab = NULL,col="blue")}
par(mfrow = c(1,2))
for(i in c(2,8)){plot(Auto[,i], main = names[i], col = "green")}
#boxplot
boxplot(Auto$mpg~Auto$origin,col=hcl(240), main="mpg ~ cylinders")
##Incisos del ejercicio 9
#a
pairs(Auto, cex = 1.2, pch = '.', col = "#1c7178", main = "Scatterplot Matrix", cmi = c(1,3,1,3))
#b
kable(cor(Auto[,-c(2,8,9)]))
corrplot(cor(Auto[,-c(2,8,9)]))
#c
modelos <- list()
modelos[[1]] <- lm(mpg ~ cylinders + displacement + horsepower + weight + acceleration + year + origin, data = Auto)
summary(modelos[[1]])
#d
par(mfrow = c(2,2))
plot(modelos[[1]], pch = '.', cex = 4, col="#168f99")
#e
#Interacción entre year:origin
modelos[[2]] <- lm(mpg ~ cylinders + displacement + horsepower + weight + acceleration + year + origin + year:origin, data = Auto)
summary(modelos[[2]])
#Interacción weight:acceleration
modelos[[3]] <- lm(mpg ~ cylinders + displacement + horsepower + weight + acceleration + weight:acceleration + year + origin, data = Auto)
summary(modelos[[3]])
#Interacción cylinders:displacement
modelos[[4]] <- lm(mpg ~ cylinders + displacement + cylinders:displacement + horsepower + weight + acceleration + year + origin, data = Auto)
summary(modelos[[4]])
#Interacción weight:acceleration y cylinders:displacement
modelos[[5]] <- lm(mpg ~ cylinders + displacement + cylinders:displacement + horsepower + weight + acceleration + weight:acceleration + year + origin, data = Auto)
summary(modelos[[5]])
r2ajus <- c()
for (i in 1:5) {
  r2ajus[i] <- summary(modelos[[i]])$adj.r.squared
}
anovas <- list()
for(i in 2:5){anovas[[i-1]] <- anova(modelos[[1]],modelos[[i]])}
anovas_pvalue <- c(); for(i in 1:4){anovas_pvalue[i] <- anovas[[i]]$`Pr(>F)`[2]}
comparacion <- data.frame(modelo = 1:5,r2_adj = r2ajus, pvalue = c(NA,anovas_pvalue))
comparacion

par(mfrow = c(2,2))
plot(modelos[[1]], which = 1, pch = '.', cex = 4, col="#168f99", main = "Modelo 1")
plot(modelos[[2]], which = 1, pch = '.', cex = 4, col="#168f99", main = "Modelo 2")
plot(modelos[[3]], which = 1, pch = '.', cex = 4, col="#168f99", main = "Modelo 3")
plot(modelos[[4]], which = 1, pch = '.', cex = 4, col="#168f99", main = "Modelo 4")

#f
# Tranformaciones sobre las variables predictoras.
#Tomar el log de la variable displacement
modelos[[6]] <- lm(mpg ~ cylinders + sqrt(displacement) + horsepower + weight + acceleration + weight:acceleration + year + origin, data = Auto)
summary(modelos[[6]])
modelos[[7]] <- lm(mpg ~ cylinders + displacement + horsepower^2 + weight + acceleration + weight:acceleration + year + origin, data = Auto)
summary(modelos[[7]])
modelos[[8]] <- lm(mpg ~ cylinders + displacement + horsepower + weight + weight:acceleration + log(acceleration) + year + origin, data = Auto)
summary(modelos[[8]])

for (i in 6:8) {
  r2ajus[i] <- summary(modelos[[i]])$adj.r.squared
}
for(i in 6:8){anovas[[i-1]] <- anova(modelos[[1]],modelos[[i]])}
for(i in 5:7){anovas_pvalue[i] <- anovas[[i]]$`Pr(>F)`[2]}
comparacion <- data.frame(modelo = 1:8,r2_adj = r2ajus, pvalue = c(NA,anovas_pvalue))
comparacion
par(mfrow = c(2,2))
plot(modelos[[1]], which = 1, pch = '.', cex = 4, col="#168f99", main = "Modelo 1")
plot(modelos[[4]], which = 1, pch = '.', cex = 4, col="#168f99", main = "Modelo 4")
plot(modelos[[6]], which = 1, pch = '.', cex = 4, col="#168f99", main = "Modelo 6")
plot(modelos[[8]], which = 1, pch = '.', cex = 4, col="#168f99", main = "Modelo 8")

#1) EJERCICIO15######
data("Boston")
###Exploración de la base####
str(Boston)
summary(Boston)
#No hay datos faltantes en la base
for (i in 1:14) {print(anyNA(Boston[,i]))}
#Convertimos las variables categóricas a factores
Boston$chas <- as.factor(Boston$chas); Boston$rad <- as.factor(Boston$rad)
str(Boston)
namesBos <- names(Boston); factBos <- c(4,9); numBos <- c(2,3,5,6,7,8,10,11,12,13,14)
#Histogramas de las variables predictoras
par(mfrow = c(2,3))
for(i in numBos){hist(Boston[,i], main = namesBos[i], xlab = NULL,col="#be9cba")}
par(mfrow = c(1,2))
for(i in factBos){plot(Boston[,i], main = namesBos[i], col = "#3f233c")}
#boxplot con el log de crim
boxplot(log(Boston$crim)~Boston$chas,col="#a97ca4", main="crim ~ chas")
boxplot(log(Boston$crim)~Boston$rad,col="#a97ca4", main="log(crim) ~ rad")
#Graficas de pares
pairs(Boston, cex = 1.2, pch = '.', col = "#7d3e79", main = "Scatterplot Matrix")
#Matriz de correlaciones
cor(Boston[,-factBos]); corrplot(cor(Boston[,-factBos]))
###Construccón de los modelos####
modelosBoston <- list(); for(i in 2:14){modelosBoston[[i-1]] <- lm(Boston$crim ~ Boston[,i])}
names(modelosBoston) <- namesBos[2:14]
par(mfrow = c(2,2))
for (i in c(1,2,4,9,12,13)) {
  plot(x = Boston[,i+1],y=Boston$crim, xlab = namesBos[i+1], ylab = "crim rate", main = paste0("crim ~ ", namesBos[i+1]),cex = 5, pch = '.', col = "#7d3e79") 
  abline(modelosBoston[[i]], col="blue")
}
#Resumen de los modelos
resumenesBos <- lapply(modelosBoston, summary)
for(i in 1:13){resumenesBos[[i]]$call = paste0("lm(formula = Boston$crim ~ ",namesBos[i+1],")")}
r2 <- sapply(resumenesBos, function(x){x$r.squared}) #R2
sig_est <- sapply(resumenesBos, function(x){x$sigma}) #estimaciÃ³n de sigma (RSS)
intrcpt <- sapply(resumenesBos, function(x){x$coefficients[1,1]}) #B0 - intercepto
pvl_b0 <- sapply(resumenesBos, function(x){x$coefficients[1,4]}) #p-value para prueba t B0
b1 <- sapply(resumenesBos, function(x){x$coefficients[2,1]}) #B1 - variable predictora
pvl_b1 <- sapply(resumenesBos, function(x){x$coefficients[2,4]}) #p-value para prueba t B1
resumen <- data.frame( R2 = r2,sigma_estimada = sig_est, 
                      Intercepto = intrcpt, p_value_B0 = pvl_b0, beta_1 = b1, p_value_B1 = pvl_b1)
kable(resumen)
##Gráficas de los errores
par(mfrow = c(2,3))
for (i in c(1,2,4,9,12,13)) {
  plot(modelosBoston[[i]], which = 1, main = paste0("crim ~ ", namesBos[i+1]),cex = 5, pch = '.', col = "#7d3e79") 
}
for (i in c(1,2,4,9,12,13)) {
  plot(modelosBoston[[i]], which = 2, main = paste0("crim ~ ", namesBos[i+1]),cex = 5, pch = '.', col = "#7d3e79") 
}
###Modelo de regresión lineal múltiple####
modeloBos_multiple <- lm(crim~., data = Boston); summary(modeloBos_multiple)
par(mfrow = c(2,2))
plot(modeloBos_multiple,cex = 5, pch = '.', col = "#673564")
###Aplicando la transformación logaritmo####
modelosBostonlog <- list(); for(i in 2:14){modelosBostonlog[[i-1]] <- lm(log(Boston$crim) ~ Boston[,i])}
names(modelosBostonlog) <- namesBos[2:14]
par(mfrow = c(1,3))
for (i in c(9,12,13)) {
  plot(x = Boston[,i+1],y=log(Boston$crim), xlab = namesBos[i+1], ylab = "log(crim rate)", main = paste0("log(crim) ~ ", namesBos[i+1]),cex = 5, pch = '.', col = "#a10684") 
  abline(modelosBostonlog[[i]], col="#293133",lwd = 2)
}

#Resumen de los modelos
resumenesBoslog <- lapply(modelosBostonlog, summary)
for(i in 1:13){resumenesBoslog[[i]]$call = paste0("lm(formula = log(Boston$crim) ~ ",namesBos[i+1],")")}
r2log <- sapply(resumenesBoslog, function(x){x$r.squared}) #R2
sig_est_log <- sapply(resumenesBoslog, function(x){x$sigma}) #estimaciÃ³n de sigma (RSS)
intrcpt_log <- sapply(resumenesBoslog, function(x){x$coefficients[1,1]}) #B0 - intercepto
pvl_b0_log <- sapply(resumenesBoslog, function(x){x$coefficients[1,4]}) #p-value para prueba t B0
b1_log <- sapply(resumenesBoslog, function(x){x$coefficients[2,1]}) #B1 - variable predictora
pvl_b1_log <- sapply(resumenesBoslog, function(x){x$coefficients[2,4]}) #p-value para prueba t B1
resumen_log <- data.frame( R2 = r2log,sigma_estimada = sig_est_log, 
                       Intercepto = intrcpt_log, p_value_B0 = pvl_b0_log, beta_1 = b1_log, p_value_B1 = pvl_b1_log)
kable(resumen_log)
##Gráficas de los errores
par(mfrow = c(2,3))
for (i in c(9,12,13)) {plot(modelosBoston[[i]], which = 2, main = paste0("crim ~ ", namesBos[i+1]),cex = 5, pch = '.', col = "#7d3e79")}
for (i in c(9,12,13)) {plot(modelosBostonlog[[i]], which = 2, main = paste0("log(crim) ~ ", namesBos[i+1]),cex = 5, pch = '.', col = "#a10684") }
###Múltiple
modeloBos_multiple_log <- lm(log(crim)~., data = Boston); summary(modeloBos_multiple_log)
par(mfrow = c(2,2))
plot(modeloBos_multiple, which = c(1,2), cex = 5, pch = '.', col = "#673564")
plot(modeloBos_multiple_log, which = c(1,2), cex = 5, pch = '.', col = "#773974")
###Comparación de los parámetros####
x_est <- resumen$beta_1
y_est <- summary(modeloBos_multiple)$coefficients[-1,1] #solo comparamos los b1 de 
                                                #niveles de referencia de las variables categórica
x_est_log <- resumen_log$beta_1
y_est_log <- summary(modeloBos_multiple_log)$coefficients[-1,1] 
par(mfrow = c(1,2))
plot(x_est,y_est, xlab = "beta_1 del modelo simple", ylab = "beta del modelo multiple",
     main = "Parámetros estimados",cex = 5, pch = '.', col = "#6c2f69")
text(x_est, y_est,
     labels = namesBos[-1],
     cex = 0.6, pos = 4, col = "red")
plot(x_est_log,y_est_log, xlab = "beta_1 del modelo simple", ylab = "beta del modelo multiple ",
     main = "Parámetros estimados con transformación log",cex = 5, pch = '.', col = "#a97ca4")
text(x_est_log, y_est_log,
     labels = namesBos[-1],
     cex = 0.6, pos = 4, col = "red")
###Modelos con transformaciones no lineales####
modelos_nl <- list(); for(i in numBos){modelos_nl[[i]] <- lm(Boston$crim ~ Boston[,i] + I(Boston[,i]^2) + I(Boston[,i]^3))}
modelos_nl <- Filter(Negate(is.null),modelos_nl); names(modelos_nl) <- namesBos[numBos]
resumenes_nl <- lapply(modelos_nl, summary)
r2 <- sapply(resumenes_nl, function(x){x$r.squared}) #R2
sig_est <- sapply(resumenes_nl, function(x){x$sigma}) #estimaciÃ³n de sigma (RSS)
intrcpt <- sapply(resumenes_nl, function(x){x$coefficients[1,1]}) #B0 - intercepto
b1 <- sapply(resumenes_nl, function(x){x$coefficients[2,1]}) #B1 - variable predictora
pvl_b1 <- sapply(resumenes_nl, function(x){x$coefficients[2,4]}) #p-value para prueba t B1
b2 <- sapply(resumenes_nl, function(x){x$coefficients[3,1]}) #B2 - variable predictora
pvl_b2 <- sapply(resumenes_nl, function(x){x$coefficients[3,4]}) #p-value para prueba t B2
b3 <- sapply(resumenes_nl, function(x){x$coefficients[4,1]}) #B3 - variable predictora
pvl_b3 <- sapply(resumenes_nl, function(x){x$coefficients[4,4]}) #p-value para prueba t B3
resumen_nl <- data.frame( R2 = r2,sigma_estimada = sig_est, 
                       Intercepto = intrcpt, beta_1 = b1, p_value_B1 = pvl_b1,
                       beta_2 = b2, p_value_B2 = pvl_b2,beta_3 = b3, p_value_B3 = pvl_b3)
kable(resumen_nl)
par(mfrow=c(2,4))
ggplot(augment(modelos_nl[[i]]),aes(x = Boston...i.,y=.fitted)) + geom_line() + xlab(namesBos[numBos[i]]) + ylab("crim") +
    geom_point(aes(x=Boston[,numBos[i]],y=crim))
ggplot(augment(modelos_nl[[3]]),aes(x = Boston...i.,y=.fitted)) + geom_line() + xlab(namesBos[numBos[]]) + ylab("crim") +
  geom_point(aes(x=Boston[,numBos[3]],y=Boston$crim))
##########
par(mfrow = c(2,2))
m2 <- lm(log(Boston$crim) ~ Boston$zn)
plot(m2);summary(m2)

ggplot(Boston, aes(x = chas, y = crim)) +
  geom_point() +
  geom_line(augment(modelosBoston[[4]]), aes(y = .fitted, color = Boston...i.))


#2) EJERCICIO VARIABLES CATEGÓRICAS
###Exploración de la base####
june_13 <- read.csv("C:\\Users\\Yanely\\Documents\\2021-1\\Aprendizaje Estadístico Automatizado - Seminario de Estadística\\June_13_data.csv")
str(june_13)
june_13 <-select(june_13,Crash_Score,Time_of_Day,Rd_Feature,Rd_Configuration,Traffic_Control)
for (i in 2:5) {june_13[,i] <- as.factor(june_13[,i])};str(june_13)
par(mfrow=c(2,2)); for (i in 2:5) {plot(june_13[,i], col = "#008f39")}
for (i in 2:5) {boxplot(june_13$Crash_Score ~ june_13[,i], col = "#4c9141", main = names(june_13[i]))}
###Ajuste del modelo múltiple####
modelos_jun13 <- list()
modelos_jun13[[1]] <- lm(Crash_Score~.,data = june_13);summary(modelos_jun13[[1]])
par(mfrow=c(2,2)); plot(modelos_jun13[[1]], col = "#52b26c",pch=".")
#con transformación boxcox
par(mfrow = c(1,1)); bc <- boxcox(modelos_jun13[[1]],plotit = TRUE)
lambda <- bc$x[which.max(bc$y)] #lambda=0.263
#Como lambda=0.263 optaremos por usar la transformación logaritmo (lambda=0)
modelos_jun13[[2]] <- lm((Crash_Score)^lambda ~.,data = june_13);summary(modelos_jun13[[2]])
modelos_jun13[[3]] <- lm(log(Crash_Score) ~.,data = june_13);summary(modelos_jun13[[3]])
###Gráficas de los errores####
par(mfrow=c(1,3));for (i in 1:3) {plot(modelos_jun13[[i]],which=1,col="#78c288")}
par(mfrow=c(1,3));for (i in 1:3) {plot(modelos_jun13[[i]],which=2,pch=".",cex=4,col="#78c288")}
drop1(modelos_jun13[[1]],test = "F")
drop1(modelos_jun13[[2]],test = "F")
###Probamos varios modelos agrupando categorías####
drop1(modelos_jun13[[3]],test = "F")
levels(june_13$Rd_Feature)[c(1,3,4,5)]="DRIVEWAY_NONE_OTHER_RAMP"
modelos_jun13[[4]]<- lm(log(Crash_Score)~.,data = june_13);summary(modelos_jun13[[4]])
anova(modelos_jun13[[3]],modelos_jun13[[4]]) #Se acepta la prueba, nos quedamos con el modelo reducidO
levels(june_13$Rd_Configuration)[c(1,2,4,5)]="ONEWAY_TWO-WAY-NO-MEDIAN_TWO-WAY-UNPROTECTED_UNKNOWN"
modelos_jun13[[5]] <- lm(log(Crash_Score) ~.,data = june_13);summary(modelos_jun13[[5]])
anova(modelos_jun13[[4]],modelos_jun13[[5]]) #Se acepata la hipótesis nula, nos quedamos con el modelo reducido
levels(june_13$Traffic_Control)[c(1,2,5)]="NONE_OTHER_YIELD"
modelos_jun13[[6]] <- lm(log(Crash_Score) ~.,data = june_13);summary(modelos_jun13[[6]])
anova(modelos_jun13[[5]],modelos_jun13[[6]]) #Se acepata la hipótesis nula, nos quedamos con el modelo reducido
drop1(modelos_jun13[[6]], test = "F") #Drop1 del modelo final
###Gráficas de errores del modelo final####
par(mfrow = c(2,2));plot(modelos_jun13[[6]],pch=".",cex=2.5,col="#40aa96")
