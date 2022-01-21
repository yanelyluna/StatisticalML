######## TAREA 6 - ####
library(MASS)  ; library(corrplot)
library(leaps)
library(hdi); library(glmnet)
rm(list=ls())
options(digits=4, width = 120)
################## Ejercicio 1 #####################################
#        Aditivo Step k=2 Step k=log(n)/Regsubsets
#ECM Ap     0.581    0.582                    0.596
#ECM Val    0.623    0.599                    0.607
#p         13.000    9.000                    6.000
#R^2        0.875    0.875                    0.872
#R^2 Adj    0.872    0.873                    0.871
#AIC     1191.485 1184.107                 1190.067
#BIC     1254.883 1230.599                 1223.879

#-----------Los errores están en porcentaje
#                           Error aparente Error prueba
#R. Aditiva                         58.1         62.3
#Step k=2                           58.2         59.9
#Step k=log(n)/Regsubsets           59.6         60.7
#Lasso                              58.5         61.9
#Ridge                              60.4         63.2
#Elasticnet                         58.2         61.7

#-----AnÃ¡lisis exploratorio
?Boston #Base con 506 observaciones y 14 variables 
#crim:per capita crime rate by town.
#zn:proportion of residential land zoned for lots over 25,000 sq.ft.
#indus:proportion of non-retail business acres per town.
#chas: Charles River dummy variable (= 1 if tract bounds river; 0 otherwise).
#nox: nitrogen oxides concentration (parts per 10 million).
#rm: average number of rooms per dwelling.
#age:proportion of owner-occupied units built prior to 1940.
#dis:weighted mean of distances to five Boston employment centres.
#rad: index of accessibility to radial highways.
#tax: full-value property-tax rate per \$10,000.
#ptratio: pupil-teacher ratio by town.
#black:1000(Bk - 0.63)^2 where Bk is the proportion of blacks by town.
#lstat:lower status of the population (percent).
#medv:median value of owner-occupied homes in \$1000s.
str(Boston)
Boston$rad = as.numeric(Boston$rad)
Boston$chas = as.factor(Boston$chas)
str(Boston)
attach(Boston)
cor <- cor(Boston[, -4]) #Correlacion
corrplot(cor, method = "shade", title = "Correlación Boston")
require(stats)
require(graphics)
#pairs(Boston)
par(mfrow = c(2, 3))
Boston.r = Boston[,-4]
for (i in 1:13) {
  boxplot(Boston.r[, i] ~ crim, main = colnames(Boston.r)[i])
}
table(crim)
par(mfrow = c(4, 4), mar = c(4, 4, 2, 1.5))
plot(
  crim ~ medv + zn + indus + chas + nox + rm + age + dis +
    rad + tax + ptratio + black + lstat,
  pch = 19,
  cex = .5,
  ask = FALSE
)
par(mfrow = c(1, 2), mar = c(4, 4, 2, 1.5))
hist(crim, breaks = 20, prob = TRUE)
hist(log(crim), breaks = 20, prob = TRUE)

signif(cor(Boston[, -4]), digits = 1)
##################################################################
#Creamos la matriz de errores
matriz.err = matrix(data = NA, nrow = 6, ncol = 2)
colnames(matriz.err) <- c("Error aparente", "Error prueba")
rownames(matriz.err) <-
  c("R. Aditiva",
    "Step k=2",
    "Step k=log(n)/Regsubsets",
    "Lasso",
    "Ridge",
    "Elasticnet")
matriz.err

#Funcion para crear tasas erroress de prueba
ftrain <- function(form, train) {
  mtemp <- lm(form, data = Boston[train, ])
  return(mean((
    log(crim[-train]) - predict(mtemp, newdata = Boston[-train, ])
  ) ^ 2))
}
#####BOXCOX#######################################################


m0 = lm(crim ~ ., data = Boston)
drop1(m0, test = "F")
summary(m0) #Adjusted R-squared:  0.44
par(mfrow = c(1, 2))
plot(m0,
     which = 1:2,
     pch = 19,
     cex = .5)

boxcox(lm(crim ~ ., data = Boston), plotit = T)
bc = boxcox(m0)
(lambda = bc$x[which.max(bc$y)]) # lambda= 0.0202

#usaremos la transformación log(crim)


#####Regressión múltiple con efectos aditivos#####################
M = list(4)
M[[1]] = lm(log(crim) ~ ., data = Boston)
drop1(M[[1]], test = "F") #Todas sig.-medv, -tax, -chas, -rm, -dis
summary(M[[1]]) #Adjusted R-squared:   0.872
par(mfrow = c(1, 2), mar = c(4.5, 4.5, 4, 1))
plot(M[[1]],
     which = 1:2,
     pch = 19,
     cex = .5)
par(mfrow = c(4, 4), mar = c(2, 2, 2, 1))
# TransformaciÃ³n logaritmo
sapply(2:14, function(i) {
  plot(
    Boston[, i],
    log(crim),
    pch = ".",
    ylab = "",
    main = names(Boston)[i]
  )
})
par(mfrow = c(1, 1), mar = c(5.1, 4.1, 4.1, 2.1))

#############################      AJUSTE DE MODELOS      ##################################

####Seleccionar 3 modelos con "regsubsets", "step" k=2 y k=log(n)#
#--regsubsets, method=exahustive
#--step k=2
M[[2]] = step(lm(log(crim) ~ ., data = Boston), trace = 0, k = 2)
drop1(M[[2]], test = "F")
#log(crim) ~ zn + indus + nox + age + rad + ptratio + black + lstat + medv
#Todas sig. -medv

summary(M[[2]]) #Adjusted R-squared:  0.873
dev.off()
par(mfrow = c(1, 2), mar = c(4.5, 4.5, 4, 1))
plot(M[[2]],
     which = 1:2,
     pch = 19,
     cex = .5)

#--step k=log(n)
M[[3]] = step(lm(log(crim) ~ ., data = Boston), trace = 0, k = log(506))

drop1(M[[3]], test = "F")
#log(crim) ~ zn + nox + age + rad + black + lstat
#Todas significativas

summary(M[[3]]) #Adjusted R-squared: 0.871
par(mfrow = c(2, 2), mar = c(4, 4, 2, 1.5))
plot(M[[3]],
     which = 1:4,
     pch = 19,
     cex = .3)
par(mfrow = c(1, 2))
plot(
  M[[3]]$fitted,
  log(medv),
  pch = 19,
  cex = .3,
  main = "escala log",
  cex.main = 1
)
abline(0, 1, col = "red")
plot(
  exp(M[[3]]$fitted),
  medv,
  pch = 19,
  cex = .3,
  main = "escala ",
  cex.main = 1
)
abline(0, 1, col = "red")
par(mfrow = c(1, 1))

#--regsubsets, method=exahustive
mbest <- regsubsets(log(crim) ~ ., data = Boston, nvmax = 13)
summary(mbest)

plot(
  mbest$rss,
  pch = 19,
  col = "red",
  xlab = "subset of size k",
  ylab = "Residual sum of squares"
)
coef(mbest, 1)
coef(mbest, 1:2)

summary(regsubsets(
  log(crim) ~ .,
  data = Boston,
  nbest = 1,
  nvmax = 13
))
summary(regsubsets(
  log(crim) ~ .,
  data = Boston,
  nbest = 1,
  nvmax = 13,
  force.in = c(6, 13)
))

reg.best = regsubsets(
  log(crim) ~ .,
  data = Boston,
  nbest = 1,
  nvmax = 13,
  method = "exhaustive"
)

exhaustive = coef(reg.best, 11)
aux.summary = summary(reg.best)
names(aux.summary)

round(cbind(rsq = aux.summary$rsq, adjr2 = aux.summary$adjr2), 4)

# A graphical way of looking at these models:

par(mfrow = c(2, 2), mar = c(4, 4, 2, 1.5))
plot(aux.summary$rss,
     xlab = "Number of variables",
     ylab = "RSS",
     type = "b")
(which.min(aux.summary$rss))
points(13,
       aux.summary$rss[which.min(aux.summary$rss)],
       col = "red",
       cex = 2,
       pch = 20)
plot(aux.summary$adjr2,
     xlab = "Number of variables",
     ylab = "Adj R^2",
     type = "b")
(which.max(aux.summary$adjr2))
points(9,
       aux.summary$adjr2[which.max(aux.summary$adjr2)],
       col = "red",
       cex = 2,
       pch = 20)
plot(aux.summary$cp,
     xlab = "Number of variables",
     ylab = "Cp",
     type = "b")
(which.min(aux.summary$cp))
points(9,
       aux.summary$cp[which.min(aux.summary$cp)],
       col = "red",
       cex = 2,
       pch = 20)
plot(aux.summary$bic,
     xlab = "Number of variables",
     ylab = "BIC",
     type = "b")
(which.min(aux.summary$bic))
points(6,
       aux.summary$bic[which.min(aux.summary$bic)],
       col = "red",
       cex = 2,
       pch = 20)
#Menor Bic  obtenido es con 6 variables
(ps = colnames(Boston)[aux.summary$which[which.min(aux.summary$bic), ]][-1])
# FÃ³rmulas
(f12 = formula(paste0("log(crim)~", paste(ps, collapse = "+"))))


# Generamos el modelo obtenido por Best Subset
M[[4]] = lm(f12, data = Boston) #log(crim) ~ zn + nox + age + rad + black + lstat
summary(M[[4]])#R cuadrada 0.871
#Vemos que M4 y M3 son el mismo modelo
####Seleccionar 3 modelos con "lasso", "ridge" , "elasticnet"####

##se calcula el error cuadrÃ¡tico medio, no calculamos M[4] porser el mismo que
#M[3]

(matriz.err[1:3, 1] = sapply(M[c(1, 2, 3)],
                             function(m) {
                               mean((log(crim) - fitted(m)) ^ 2)
                             }))
##se calcula el error prueba
set.seed(34)
for (i in 1:3) {
  matriz.err[i, 2] = mean(replicate(300, ftrain(formula(M[[i]]), sample(1:506, 506 *
                                                                          (
                                                                            4 / 5
                                                                          )))))
}
matriz.err


# Cuadro comparativo de los modelos
# ECM Aparente y Validado, número de variables, R2 múltiple y ajustada, AIC y BIC

table = rbind(t(matriz.err[1:3, ]), sapply(M[c(1, 2, 3)], function(m) {
  aux = summary(m)
  return(c(aux$df[1] - 1,
           aux$r.squared, aux$adj.r.squared, AIC(m), BIC(m)))
}))
colnames(table) = c('Aditivo', 'Step k=2', 'Step k=log(n)/Regsubsets')
rownames(table) = c('ECM Ap', 'ECM Val', 'p', 'R^2', 'R^2 Adj', 'AIC', 'BIC')
round((table), 3)
####Seleccionar 3 modelos con "lasso", "ridge" , "elasticnet"####
# Matrix model para regresión Ridge y Lasso
library(glmnet)
x = model.matrix(log(crim) ~ . , data = Boston) #para construir la matriz diseño
y = log(Boston$crim) #variable respuesta

str(x)

modelos <- list(3)
err = matrix(NA, ncol = 6, nrow = 3)
rownames(err) = c("lasso", "ridge", "elastic net")
colnames(err) = c("TrnErr(Ap)",
                  "  cv(k=10, B=1)",
                  "cvsd",
                  "  #betas(df)",
                  "lambda.min",
                  "lambda.1se")
err
Mcv <- list(3)
#Lasso, Ridge y elastic net
al <- c(1, 0, 0.5)
modelos = lapply(al, function(al) {
  glmnet(x, y, alpha = al)
})
summary(modelos[[1]])

set.seed(1234)
t1 = proc.time()
for (Nmod in 1:3) {
  Mcv[[Nmod]] = cv.glmnet(x,
                          y,
                          type.measure = "mse",
                          nfolds = 10,
                          alpha = al[Nmod])
  err[Nmod, 1] = assess.glmnet(modelos[[Nmod]],
                               s = Mcv[[Nmod]]$lambda.min,
                               newx = x,
                               newy = y)$mse #error aparente
  err[Nmod, 2] = Mcv[[Nmod]]$cvm[which(Mcv[[Nmod]]$lambda == Mcv[[Nmod]]$lambda.min)] #error por cv
  # del modelo con lambda min
  err[Nmod, 3] = Mcv[[Nmod]]$cvsd[which(Mcv[[Nmod]]$lambda == Mcv[[Nmod]]$lambda.min)] #error estándar
  err[Nmod, 4] = Mcv[[Nmod]]$nzero[which(Mcv[[Nmod]]$lambda == Mcv[[Nmod]]$lambda.min)] +
    1 #betas que no son cero
  err[Nmod, 5] = Mcv[[Nmod]]$lambda.min #valor de lambda min
  err[Nmod, 6] = Mcv[[Nmod]]$lambda.1se
}
proc.time() - t1
err
matriz.err[4:6, 1:2] <- err[1:3, 1:2]
matriz.err
(round(matriz.err * 100, 1))

#Valores de las betas del modelo ajustado
aux <- function(i) {
  a <-
    modelos[[i]]$beta[, which(modelos[[i]]$lambda == Mcv[[i]]$lambda.min)]
  a <- a[a != 0]
}
betas <- lapply(1:3, aux)
#Lasso
hist(betas[[1]])
b1 <- betas[[1]][abs(betas[[1]]) > 0.015]
length(b1)
err
modelos[[1]]
plot(modelos[[1]])
par(mfrow = c(1, 2))
plot(modelos[[1]], xvar = "lambda")
plot(Mcv[[1]])
#Ridge
plot(modelos[[2]], xvar = "lambda")
plot(Mcv[[2]])
#Elastic net 0.5
plot(modelos[[3]], xvar = "lambda")
plot(Mcv[[3]])


##################### Ejercicio 2  ###############################

? riboflavin
data("riboflavin")
# Matrix model
x = model.matrix(y ~ . , data = riboflavin) #para construir la matriz diseño
y = riboflavin$y #variable respuesta

str(x) #4088 variables y el intercepto

modelos <- list(3)
err = matrix(NA, ncol = 6, nrow = 3)
rownames(err) = c("ridge", "lasso", "elastic net")
colnames(err) = c("TrnErr(Ap)",
                  "  cv(k=10, B=1)",
                  "cvsd",
                  "  #betas(df)",
                  "  lambda.min",
                  "  cv(k=10, B=100)")
err
Mcv <- list(3)
#Ridge, Lasso y elastic net
al <- c(0, 1, 0.5)

set.seed(1234)
t1 = proc.time()
for (Nmod in 1:3) {
  modelos[[Nmod]] <- glmnet(x, y, alpha = al[Nmod])
  Mcv[[Nmod]] = cv.glmnet(x,
                          y,
                          type.measure = "mse",
                          nfolds = 10,
                          alpha = al[Nmod])
  err[Nmod, 1] = assess.glmnet(modelos[[Nmod]],
                               s = Mcv[[Nmod]]$lambda.min,
                               newx = x,
                               newy = y)$mse #error aparente
  err[Nmod, 2] = Mcv[[Nmod]]$cvm[which(Mcv[[Nmod]]$lambda == Mcv[[Nmod]]$lambda.min)] #error por cv
  # del modelo con lambda min
  err[Nmod, 3] = Mcv[[Nmod]]$cvsd[which(Mcv[[Nmod]]$lambda == Mcv[[Nmod]]$lambda.min)] #error estándar
  err[Nmod, 4] = Mcv[[Nmod]]$nzero[which(Mcv[[Nmod]]$lambda == Mcv[[Nmod]]$lambda.min)] +
    1 #betas que no son cero
  err[Nmod, 5] = Mcv[[Nmod]]$lambda.min #valor de lambda que min el mse
}
proc.time() - t1
err

coef(Mcv[[2]])

#Numero de betas distintas de cero y valor de lambda según el criterio
auxf <- function(i, crit) {
  if (crit == "lambda.min") {
    a <-
      modelos[[i]]$beta[, which(modelos[[i]]$lambda == Mcv[[i]]$lambda.min)]
    a <- a[a != 0]
    b <- Mcv[[i]]$lambda.min
  }
  if (crit == "lambda.1se") {
    a <-
      modelos[[i]]$beta[, which(modelos[[i]]$lambda == Mcv[[i]]$lambda.1se)]
    a <- a[a != 0]
    b <- Mcv[[i]]$lambda.1se
  }
  if (crit == "df") {
    a <-
      modelos[[i]]$beta[, which(modelos[[i]]$df == max(modelos[[i]]$df))[1]]
    a <- a[a != 0]
    b <-
      modelos[[i]]$lambda[modelos[[i]]$df == max(modelos[[i]]$df)[1]][1]
  }
  return(c(length(a) + 1, b))
}
(betas <- sapply(1:3, auxf, crit = "lambda.min"))
(betas_lambda1se <- sapply(1:3, auxf, crit = "lambda.1se"))
(betas_df <- sapply(1:3, auxf, crit = "df"))
## GRÁFICAS
#Ridge
m <- c("ridge", "lasso", "elastic net")
auxf2 <- function(i, x) {
  betas <-
    modelos[[i]]$beta[, which(modelos[[i]]$lambda == Mcv[[i]]$lambda.min)]
  betas <- betas[betas != 0]
  b <- betas[abs(betas) > x]
  hist(
    betas,
    main = paste("Coeficientes", m[i]),
    col = "#008080",
    breaks = 40
  )
  legend("topright", legend = paste(length(b), "abs(betas) mayores a", x))
  return(b)
}
#par(mfrow=c(1,1))
betasMayores <- list(3)
betasMayores <- lapply(1:3, auxf2, x = 0.025)
betasMayores <- lapply(betasMayores, abs)
betasMayores <- lapply(betasMayores, sort, decreasing = TRUE)
betasMayores[[1]][1:6]
betasMayores[[2]][1:6]
betasMayores[[3]][1:6]

plot(modelos[[1]])
par(mfrow = c(1, 2))
plot(modelos[[1]], xvar = "lambda")
plot(Mcv[[1]])
#Lasso
plot(modelos[[2]], xvar = "lambda")
plot(Mcv[[2]])
#Elastic net 0.5
plot(modelos[[3]], xvar = "lambda")
plot(Mcv[[3]])

##Validación del error con CV B=100
B = 100
Mcvaux <- list(3)
aux = matrix(NA, nrow = B, ncol = 6)
set.seed(1234)
t1 = proc.time()
for (Nmod in 1:3) {
  for (Nrep in 1:B) {
    Mcvaux[[Nmod]] = cv.glmnet(x,
                               y,
                               type.measure = "mse",
                               nfolds = 10,
                               alpha = al[Nmod])
    aux[Nrep, Nmod] = Mcvaux[[Nmod]]$cvm[which(Mcvaux[[Nmod]]$lambda == Mcvaux[[Nmod]]$lambda.min)] #error por cv
    #aux[Nmod,Nmod+3]=Mcvaux[[Nmod]]$cvsd[which(Mcvaux[[Nmod]]$lambda==Mcvaux[[Nmod]]$lambda.min)] #error estándar por cv
  }
  err[Nmod, 6] = mean(aux[, Nmod])
}
proc.time() - t1
#user  system elapsed
#728.39   97.83  865.44


