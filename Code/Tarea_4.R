##SEMINARIO DE ESTADÍSTICA: STATISTICAL MACHINE LEARNING
### TAREA 4: APRENDIZAJE SUPERVISADO ###
library(MASS); library(caret);library(corrplot);options(digits = 4)
library("performance")
library("prediction")
#install.packages("performance")
#install.packages("prediction")
#######################################
#Funcion curvas ROC
AUC = function(m, train) {
  pred = predict(m, datos1[-train,], type = "response")
  return(performance(prediction(pred, datos1$type[-train]), "auc")@y.values[[1]]) }
#######################################

colores <- c("#57bb00","#3b3388","red")
datos1 <- rbind(Pima.tr,Pima.te); ?Pima.tr
# -- ANÁLISIS EXPLORATORIO
str(datos1) #532 observaciones
levels(datos1$type) <- c("0","1")
attach(datos1)
cor <- cor(datos1[,1:7]) #Correlacion
corrplot(cor)
pairs(x = datos1[, 1:7],
      col = colores[1:2][datos1$type], pch = 19) #Verde="No"
pca1 <- prcomp(datos1[,1:7],scale. = TRUE) #Componentes principales
biplot(pca1, scale=0, col=c("black", "darkorange3"))
pca1_S <- summary(pca1); pca1_S$importance[,1:5] #Primeros 5 componentes
plot(predict(pca1)[,1:2], col = colores[datos1$type],pch=19,cex=1.5, main="Componentes principales")
par(mfrow=c(2,4))
for (i in 1:7) {
  boxplot(datos1[,i]~ type,data = datos1, ylab = colnames(datos1)[i],border=colores) 
}
pvalue_norm <- matrix(NA,nrow = 7,ncol = 2); rownames(pvalue_norm)<- colnames(datos1)[1:7]
colnames(pvalue_norm)<- levels(datos1$type)
for (k in 1:7) {
  j0 <- names(datos1)[k]
  x0 <- seq(min(datos1[, k]), max(datos1[, k]), le = 50)
  for (i in 1:2) {
    i0 <- levels(datos1$type)[i]
    x <- datos1[datos1$type == i0, j0]
    hist(x, proba = T, col = "#7fb5b5", main = paste("type", i0), xlab = j0)
    lines(x0, dnorm(x0, mean(x), sd(x)), col = "red", lwd = 2)
    pvalue_norm[k,i] <- shapiro.test(x)$p.value
  } 
}
## a) Análisis de discriminante lineal ####
lda <- lda(type ~ ., datos1); lda
plot(lda)
(tc<-table(type, predict(lda)$class))
errg <- (tc[1,2]+tc[2,1])/532 #Tasa de error de clasificación global aparente
err0 <- tc[1,2]/sum(tc[1,]) #Tasa de calsificación erronea de falsos positivos
err1 <- tc[2,1]/sum(tc[2,]) #Tasa de calsificación erronea de falsos negativos
# CÁLCULO DE TASAS DE ERROR DE CLASIFICACIÓN (%)
tasas_error = function(y, prediccion) {
  return(c(mean(y != prediccion), mean(y[y==0] != prediccion[y==0]), 
           mean(y[y==1] != prediccion[y==1])) * 100) }
# Error global, error grupo 1, error grupo 0 #tasas_error(type,predict(lda)$class)
### Ajustando el modelo en un conjunto entrenamiento 
#0.8*532 = 425.6
set.seed(1120)
table(datos1$type)
training_set <- createDataPartition(datos1$type,p=0.8)$Resample1
table(datos1$type[training_set])
lda_tr <- lda(type~ . ,datos1, subset=training_set)
lda_pred <- predict(lda_tr,newdata = datos1[-training_set,])
#Tasas de error sobre el conjunto entrenamiento
tr_err <- tasas_error(type[training_set],predict(lda_tr,newdata = datos1[training_set,])$class)
#Tasas de error sobre el conjunto de prueba
test_err <- tasas_error(type[-training_set],lda_pred$class)
rbind(tr_err,test_err)

#Repetimos el procedimiento B veces
set.seed(1120)
B=1000
err_tr_tst=matrix(NA,ncol=3, nrow=B)
colnames(err_tr_tst)=c("global_test","0_tst","1_test")
t=proc.time()
for (i in 1:B) {
  train <- createDataPartition(datos1$type,p=0.8)$Resample1
  lda_tr <- lda(type~ . ,datos1, subset=train)
  lda_pr <- predict(lda_tr,newdata = datos1[-train,])
  err_tr_tst[i,] <- tasas_error(type[-train],lda_pr$class)
}
(proc.time()-t) #
error_trn_tst <- colMeans(err_tr_tst); error_trn_tst
apply(err_tr_tst, 2, sd)
par(mfrow=c(1,3))
for (i in 1:3) {
  hist(err_tr_tst[,i],main=colnames(err_tr_tst)[i],xlab=colnames(err_tr_tst)[i])  
}

## b) Naive Bayes ####
library(e1071)
#####Naive######################
#e1071
#Ajustamos el modelo
model = naiveBayes(type ~., data = datos1);model
###Error aparente
# Se observan las probabilidades de pertenecer a cada grupo.
#Vemos las probabilidades de ser clasificadas al grupo "Yes" o "No"
modelprob<-predict(model,datos1, type = "raw"); modelprob
head(cbind(modelprob, datos1))

modelpred <- predict(model, datos1)
#Create a confusion matrix:
Matriz_confusion <- table(type,modelpred);Matriz_confusion
par(mfrow=c(1,1))
plot(Matriz_confusion, color="purple", main="Matriz confusión Naive Bayes")

a_er <- tasas_error(type, modelpred); a_er
#a_er(type, modelpred)
#            0     1
#23.87218 18.59155 34.46328 

###Training Test 0.8*532
set.seed(1120)
training_nb <- createDataPartition(datos1$type,p=0.8)$Resample1
#0    1 
#267 133 
table(datos1$type[training_nb])
model_tr <- naiveBayes(type~ ., data=datos1[training_nb,])
model_pred <- predict(model_tr,newdata =datos1[-training_nb,])
tasas_error(type[-training_nb],model_pred)
#Hacemos test-training con B=1000 veces
err_test.training=matrix(NA,ncol=3, nrow=1000)
colnames(err_test.training)=c("global.tst","No.tst","Yes.tst")
set.seed(1120)
for (i in 1:1000) {
  train <- sample(532,426)#createDataPartition(datos1$type,p=0.8)$Resample1
  model_tr <- naiveBayes(type~ ., datos1, subset=train)
  model_pr <- predict(model_tr,newdata = datos1[-train,])
  err_test.training[i,] <- tasas_error(type[-train],model_pr)
}
error_tst <- colMeans(err_test.training); error_tst

#Construimos matriz de errores 
errores_nb=matrix(NA, ncol=3, nrow=2)
colnames(errores_nb)=c("Global", "0","1")
rownames(errores_nb)=c("Aparentes NaiveB", "Test B=1000")

errores_nb[1,1:3] <- tasas_err(type, modelpred)
errores_nb[2,1:3] <- colMeans(err_test.training[,4:6])

errores_nb  
## c) Regresión logística ####
#Modelo con todas las variables
mod1 <- glm(type ~ ., data=datos1, family=binomial(link = "logit"))
summary(mod1) #bp y skin no son significativas.
drop1(mod1,test = "Chisq")
anova(mod1,test = "Chisq")
#Modelo sin skin
mod2 <- glm(type ~ .-skin, data=datos1, family=binomial(link = "logit"));summary(mod2)
anova(mod1,mod2,test = "Chisq") #Nos quedamos con el modelo sin skin
#Modelo sin skin ni bp
mod3 <- glm(type~.-skin - bp, data = datos1,family = binomial(link = "logit"));summary(mod3)
anova(mod2,mod3, test = "Chisq") #Nos quedamos con el mod de menos variables
#Modelo sin skin, bp ni age
mod4 <- glm(type~.-skin -bp -age, data = datos1, family = binomial(link = "logit"));summary(mod4)
anova(mod3,mod4,test = "Chisq") #Nos quedamos con el mod de menos variables
mod5 <- glm(type ~ .+ age:npreg, data=datos1, family=binomial(link = "logit"))
anova(mod5,mod1,test = "Chisq") #La interacción no es significativa
#Evaluación de modelos
mod <- list(mod1,mod2,mod3,mod4,mod5)
tasas_error_rl = function(modelo, train, corte) {
  if (is.null(train)) {
    pred = as.numeric(predict(modelo, type = "response") > corte)
    y = datos1$type }
  else {
    pred = as.numeric(predict(modelo, newdata = datos1[-train,],
                              type = "response") > corte)
    y = datos1$type[-train] }
  return(100*c(mean(y != pred), mean(y[y == 0] != pred[y == 0]), 
               mean(y[y == 1] != pred[y == 1]))) }
# Error global, error grupo 1, error grupo 0.
comparacion_mod <- matrix(NA,nrow = 8,ncol = 5)
colnames(comparacion_mod) <- c("Mod1","Mod2","Mod3","Mod4","Mod5")
rownames(comparacion_mod) <- c("AIC", "BIC","Global ap", "Grupo 1 ap","Grupo 0 ap",
                               "Global te", "Grupo 1 te","Grupo 0 te")
#Con todos los datos
comparacion_mod[1,] <- sapply(mod, AIC)
comparacion_mod[2,] <- sapply(mod, BIC)
comparacion_mod[3:5,] <- sapply(mod, tasas_error_rl, train=NULL,corte=0.5)
comparacion_mod
#Training-test con un conjunto de entrenamiento del 80%
mod_tr <- list()
for (i in 1:5) {
  mod_tr[[i]] <- glm(mod[[i]]$formula, data=datos1, family=binomial(link = "logit"),subset = training_set)
}
comparacion_mod[6:8,] <- sapply(mod_tr,tasas_error_rl,train=training_set,corte=0.5)
##Elegimos el modelo 3
B = 1000
training <- replicate(B, sample(1:532,426)) #cada columna es un conjunto entrenamiento
err <- matrix(NA,nrow = B,ncol = 3); colnames(err)<- c("Global","Grupo 1", "Grupo 0")
for (i in 1:B) {
  mod_ <- glm(mod3$formula, data=datos1, family=binomial(link = "logit"),subset = training[,i])
  err[i,] <- tasas_error_rl(mod_,training[,i],0.5)
}
(tr_te_rl <- colMeans(err))

## d) SVM ####
## Ajuste de modelos con  kernel={lineal, polinomial , radial}
t<-c("linear", "polynomial", "radial")
svm.fit<-list()
tab<-list()
svm.global<-list()
svm.0<-list()
svm.1<-list()
for (i in t){
  svm.fit[[i]]=svm(type~., data=datos1 , cost=100, kernel= i,scale=T)
  tab[[i]]<- table(type, predict(svm.fit[[i]],datos1))  
  svm.global[[i]] =(1-sum(diag(tab[[i]]))/sum(tab[[i]]))* 100 
  svm.0[[i]] =((tab[[i]][1,2]/sum(tab[[i]][1,])))*100  
  svm.1[[i]] =((tab[[i]][2,1]/sum(tab[[i]][2,])))*100
}
#vemos los summaries
summary(svm.fit$linear);summary(svm.fit$polynomial);summary(svm.fit$radial)

#Sacamos la matriz de errores aparentes
#svm_aparentes
#          svm.global svm.0 svm.1
#linear     21.05      10.42  42.37
#polynomial 15.04      4.789  35.59
#radial     4.323      0.8451 11.3 
svm_aparentes=cbind(svm.global,svm.0,svm.1);svm_aparentes
#Vemos las matrices de confusión
tab
par(mfrow=c(1,3))
plot(tab$linear, color="lightgreen"); plot(tab$polynomial, color="skyblue"); plot(tab$radial, color="violet")

## Selección de modelo

set.seed(105) 
fit_lineal = tune(svm , type~., data = datos1 , kernel ="linear",
                  ranges = list(cost=c(1:3,2^(2:9))))
summary(fit_lineal)
## Mejor cost=2, menor error= 0.2198

fit_pol = tune(svm , type~., data = datos1 , kernel ="polynomial",
               ranges = list(cost=c(1:5), degree = c(2,3,4)))
summary(fit_pol)
## Mejor cost=2      degree=3  menor error=0.2443 
fit_rad = tune(svm , type~., data = datos1, kernel ="radial",
               ranges = list(cost=c(1:3, 2^(2:9)),gamma =c(0.5 ,1 ,2 ,3 ,4) ))
summary(fit_rad)
## Mejor cost=1      degree=2 menor error=0.2912 
# Mejores modelos
fit_lineal$best.model
fit_pol$best.model
fit_rad$best.model

# Mejores parametros
fit_lineal$best.parameters
fit_pol$best.parameters
fit_rad$best.parameters

# Tasas aparentes de los mejores modelos. 
#Global Grupo 0 Grupo 1
#Lineal      21.05   10.42   42.37
#Polinomial  17.86    3.66   46.33
#Radial       9.59    3.94   20.90

models = list(fit_lineal$best.model, fit_pol$best.model, fit_rad$best.model)

pred = sapply(models,function(mod){predict(mod, datos1)})
tasas_ap = sapply(1:3,function(i){tasas_error(type,pred[,i])})
table1 = as.data.frame(t(tasas_ap))
rownames(table1) = c('Lineal','Polinomial','Radial')
colnames(table1) = c("Global","Grupo 0","Grupo 1")
round(table1,2)

# Tenemos que el mejor modelo es el radial con 
# cost = 1 y gamma = 0.5
fit_rad$best.parameters

#Errores calculados por remuestreo training/test con B repeticiones.
#                         Global Grupo 0 Grupo 1
#Aparentes K-Radial        21.05   17.86   9.586
#Test K-Rad Tr/Tst B=500   22.00   45.77  10.061
#Test K-Rad Tr/Tst B=1000  22.09   45.71  10.164
#####
set.seed(1234)
B=500
errs=matrix(NA,ncol = 3, nrow = B)
prms=matrix(NA,ncol = 2, nrow = B)
colnames(prms)=c("Cost", "gamma")
t=proc.time()
for(irep in 1:B){
  train=sample(1:532, 426)
  Maux=tune(svm, type~.,data=datos1[train,], kernel="radial",
            ranges=list(cost=c(1:5), gamma=c(.001,.01,.1,.5,1)))
  Mbest=Maux$best.model
  #  print(Maux$best.parameters)
  prms[irep,1]=Maux$best.parameters[[1]]
  prms[irep,2]=Maux$best.parameters[[2]]
  tauxx=table(datos1[-train,]$type, predict(Mbest, datos1[-train,]))
  for(i in 1:2) errs[irep,i]=(1- tauxx[i,i]/sum(tauxx[i,]))
  errs[irep,3]=(1- sum(diag(tauxx))/sum(tauxx))
}
proc.time()-t
#user  system elapsed 
#6338.00   21.91 8086.11 

set.seed(1335)
B=1000
errs2=matrix(NA,ncol = 3, nrow = B)
prms2=matrix(NA,ncol = 2, nrow = B)
colnames(prms2)=c("Cost", "gamma")
t=proc.time()
for(irep in 1:B){
  train=sample(1:532, 400, replace =F)
  Maux=tune(svm, type~.,data=datos1[train,], kernel="radial",
            ranges=list(cost=c(1:5), gamma=c(.001,.01,.1,.5,1)))
  Mbest=Maux$best.model
  #  print(Maux$best.parameters)
  prms2[irep,1]=Maux$best.parameters[[1]]
  prms2[irep,2]=Maux$best.parameters[[2]]
  tauxx=table(datos1[-train,]$type, predict(Mbest, datos1[-train,]))
  for(i in 1:2) errs2[irep,i]=(1- tauxx[i,i]/sum(tauxx[i,]))
  errs2[irep,3]=(1- sum(diag(tauxx))/sum(tauxx))
}
proc.time()-t
#user   system  elapsed 
#12379.45    45.61 15173.63 

summary(prms)
summary(prms2)
errores_svm=matrix(NA, ncol=3, nrow=3)
colnames(errores_svm)=c("Global","Grupo 0", "Grupo 1")
rownames(errores_svm)=c("Aparentes K-Radial",
                        "Test K-Rad Tr/Tst B=500","Test K-Rad Tr/Tst B=1000")
errores_svm[1,]=tasas_ap[1,]
errores_svm[2,1]=mean(errs[,3])*100
errores_svm[2,2]=mean(errs[,2])*100
errores_svm[2,3]=mean(errs[,1])*100
errores_svm[3,1]=mean(errs2[,3])*100
errores_svm[3,2]=mean(errs2[,2])*100
errores_svm[3,3]=mean(errs2[,1])*100

errores_svm
#Vemos la distribución de los errores
par(mfrow=c(2,3))
hist(errs[,1],breaks=30, main="Errores 0 T-t B=500",  xlab = "Errores 0")
hist(errs[,2],breaks=30, main="Errores 1 T-t B=500", xlab="Errores 1")
hist(errs[,3],breaks=30, main="Errores Global T-t B=500", xlab="Errores Global")
hist(errs2[,1],breaks=30, main="Errores 0 T-t B=1000", xlab="Errores 0")
hist(errs2[,2],breaks=30, main="Errores 1 T-t B=1000", xlab="Errores 1")
hist(errs2[,3],breaks=30, main="Errores Global T-t B=1000", xlab="Errores Global")


##### II. Considere la base de datos cad1 del paquete gRbase.####
# install.packages("BiocManager")
# BiocManager::install("Rgraphviz")
# BiocManager::install("graph")
# BiocManager::install("RBGL")
# install.packages("gRbase")
library(gRbase)
data(cad1); str(cad1);attach(cad1)

# Coronary artery disease Data.
# A cross classified table with observational data from a Danish heart clinic. The response variable
# is CAD. A data frame with 236 observations on the following 14 variables.
# La variable respuesta es la presencia o ausencia de la Enfermedad de las Arterias Coronarias.


############################### Naive Bayes ######################################
library(ElemStatLearn)
library(MASS)
library(naivebayes)
library(caTools)
library(caret)
library(e1071)
library(boot) 
options(width=120, digits=4)

##   FUNCIONES A UTILIZAR A LO LARGO DEL EJERCICIO    ##
# Tasas de error grupos
tasa_grupos <- function(etq,pred,typ){
  return(100*mean(etq[etq == typ] != pred[etq == typ])) 
}

# Tasas de error junto a la global 
tasas_error <- function(etq,pred){
  return(c(100*mean(etq != pred),sapply(levels(CAD),function(typ){tasa_grupos(etq,pred,typ)})))
}

# Variables: 
# Sex a factor with levels          Female     Male
# AngPec a factor with levels       Atypical   None         Typical
# AMI a factor with levels          Definite   NotCertain
# QWave a factor with levels        No         Yes
# QWavecode a factor with levels    Nonusable  Usable
# STcode a factor with levels       Nonusable  Usable
# STchange a factor with levels     No         Yes
# SuffHeartF a factor with levels   No         Yes
# Hypertrophi a factor with levels  No         Yes
# Hyperchol a factor with levels    No         Yes
# Smoker a factor with levels       No         Yes
# Inherit a factor with levels      No         Yes
# Heartfail a factor with levels    No         Yes
# CAD a factor with levels          No         Yes



#Ajustamos el modelo
m = naiveBayes(CAD ~., data = cad1);m
###Error aparente
# Se observan las probabilidades de pertenecer a cada grupo.
# Vemos las probabilidades de ser clasificadas al grupo "Yes" o "No"
mprob<-predict(m,cad1[,1:13], type = "raw"); mprob
head(cbind(mprob, cad1))

mpred <- predict(m, cad1)
#Creamos la Matriz de confusion:
Matriz_confusion <- table(CAD,mpred);Matriz_confusion

par(mfrow=c(1,1))
plot(Matriz_confusion, color=c("green","red"), main="Matriz confusión Naive Bayes")

a_er <- tasas_error(CAD, mpred); a_er

###Training Test 0.8*236
##189 observaciones training
## 47 observaciones test
set.seed(16)
training_nb <- createDataPartition(cad1$CAD,p=0.8)$Resample1
table(cad1$CAD[training_nb])

m_tr <- naiveBayes(CAD~ ., cad1, subset=training_nb)
m_pred <- predict(m_tr,cad1[-training_nb])

#Hacemos test-training con B=1000 veces

err_tr.tst=matrix(NA,ncol=6, nrow=1000)
colnames(err_tr.tst)=c("global.tr","No.tr","Yes.tr","global.tst","No.tst","Yes.tst")
for (i in 1:1000) {
  train <- sample(235,189)
  m_tr <- naiveBayes(CAD~ ., cad1, subset=train)
  m_pr <- predict(m_tr,newdata = cad1[-train])
  err_tr.tst[i,1:3] <- tasas_error(CAD[train],predict(m_tr,newdata = cad1[train,]))
  err_tr.tst[i,4:6] <- tasas_error(CAD[-train],m_pr)
}
(proc.time()-t)

#user  system elapsed 
#41.365  -0.644 185.537
err_tr.tst
error_tr <- colMeans(err_tr.tst[,1:3]); error_tr
error_tst <- colMeans(err_tr.tst[,4:6]); error_tst

### CV
set.seed(1234)
B=500
erroresCV<-matrix(NA,B,3)
for (j in 1:B) {
  split <- sample.split(cad1$CAD, SplitRatio = 0.80)
  training_set <- subset(cad1, split == TRUE)
  folds <- createFolds(training_set$CAD, k = 10)
  
  cvNaiveBayes <- lapply(folds, function(x){
    training_fold <- training_set[-x, ]
    test_fold <- training_set[x, ]
    clasificador <- naiveBayes(CAD ~ ., data = cad1)
    y_pred <- predict(clasificador, newdata = test_fold)
    cm <- table(test_fold$CAD, y_pred)
    precision <- (cm[1,1] + cm[2,2]) / (cm[1,1] + cm[2,2] +cm[1,2] + cm[2,1])
    err1<-cm[1,2]/(cm[1,1]+cm[1,2])
    err2<-cm[2,1]/(cm[2,1]+cm[2,2])
    return(cbind(1-precision, err1,err2))
  })
  errcvg<-c();errcv1<-c();errcv2<-c()
  
  for(i in 1:10){
    errcvg[i]<-mean(cvNaiveBayes[[i]][1])
    errcv1[i]<-mean(cvNaiveBayes[[i]][2])
    errcv2[i]<-mean(cvNaiveBayes[[i]][3])
  }
  erroresCV[j,1]<-mean(errcvg)
  erroresCV[j,2]<-mean(errcv1)
  erroresCV[j,3]<-mean(errcv2)
}
erroresC.V<-100*c(mean(erroresCV[,1]),mean(erroresCV[,2]),mean(erroresCV[,3]))


#Construimos matriz de errores 
err_nb=matrix(NA, ncol=3, nrow=4)
colnames(err_nb)=c("Global", "0","1")
rownames(err_nb)=c("Aparentes NaiveB", "Training B=1000 ", "Test B=1000","CV")

err_nb[1,1:3] <- tasas_error(CAD, mpred)
err_nb[2,1:3] <- colMeans(err_tr.tst[,1:3])
err_nb[3,1:3] <- colMeans(err_tr.tst[,4:6])
err_nb[4,1:3] <- colMeans(erroresC.V)

err_nb 
#                  Global     0     1
# Aparentes NaiveB  13.56 11.63 15.89
# Training B=1000   13.60 11.56 16.12
# Test B=1000       48.01 38.66 57.82
# CV                13.53 11.53 15.92

##### Logistic Regression  #####

#Matriz para guardar los errores de clasificaciÃ³n
errores=matrix(NA, ncol=3, nrow=4)
colnames(errores)=c("No", "Yes", "Global")
rownames(errores)=c("Aparentes","CVdelta1", "Test B=1000")
errores

#Muestra completa: 235
#Muestra entrenadora (learning set): 188
#Muestra prueba (test set): 47
set.seed(22)
train=sample(1:235, 188); table(CAD)
table(CAD[train]) 
# I Muestra completa.

M1=glm(CAD ~., family=binomial(link=logit),data=cad1) 

summary(M1) #Null deviance: 325.11; Residual deviance:  147.73; AIC: 177.7.

deviance(M1) #147.7
AIC(M1); BIC(M1) #177.7; 229.7
deviance(M1)+log(235)*5 #175


M1PredProbs=predict(M1,type="response") #Probabilidades a posteriori

cbind(CAD,M1PredProbs)[1:10,]
table(CAD, M1$y) #Matriz de confusion

M1PredClass=ifelse(M1PredProbs>.5, "Yes", "No")
(taux=table(CAD,M1PredClass))

#Errores aparentes (training set = test set= full set)
errores[1,1]=taux[1,2]/sum(taux[1,])
errores[1,2]=taux[2,1]/sum(taux[2,])
errores[1,3]=1-sum(diag(taux))/sum(taux)
errores

#(cv.err.6 <- cv.glm(mammals, mammals.glm, K = 6)$delta)
#4-fold crossvalidation, B=1 sola repeticiÃ³n.
cv.glm(cad1, M1, K=5)$delta[1:2] 

#4-fold crossvalidation, B=1000 repeticiones 
set.seed(12)
B=1000
aux=matrix(NA,ncol = 2, nrow = B)
t=proc.time()
for(irep in 1:B){
  M1=glm(CAD ~., family=binomial(link=logit),data=cad1) 
  aux[irep,1:2]=cv.glm(cad1, M1, K=5)$delta[1:2]
}
proc.time()-t
# user  system elapsed 
# 23.39    0.08   29.82 
dev.off()
par(mfrow=c(1,2))
hist(aux[,1], breaks=47)
hist(aux[,2], breaks=47)

errores[2,3]=mean(aux[,1])
errores
##
set.seed(4)
B=1000
aux2=matrix(NA,ncol = 3, nrow = B)
t=proc.time()
for(irep in 1:B){
  train=sample(1:235, 189)
  M1Tr=glm(CAD ~., family=binomial(link=logit),
           data=cad1[train,])
  M1TrPred=predict(M1Tr,newdata=cad1[-train,],type="response")
  M1TrClass=ifelse(M1TrPred>.5, "Yes", "No")
  taux=table(CAD[-train],M1TrClass)
  aux2[irep,1]=taux[1,2]/sum(taux[1,])
  aux2[irep,2]=taux[2,1]/sum(taux[2,])
  aux2[irep,3]=1-sum(diag(taux))/sum(taux)
}
proc.time()-t
#  user  system elapsed 
# 31.00    0.13   41.19 

dev.off()
par(mfrow=c(1,2))
hist(aux[,1], breaks=47) #5-fold CV
hist(aux2[,3], breaks=47) # Tr/tst B=1000 rep.

par(mfrow=c(1,3))
hist(aux2[,1], breaks=47)
hist(aux2[,2], breaks=47)
hist(aux2[,3], breaks=47)

errores[3,1]=mean(aux2[,1])
errores[3,2]=mean(aux2[,2])
errores[3,3]=mean(aux2[,3])
errores

par(mfrow=c(1,1))
matplot(1:3,cbind(100*errores[1,],100*errores[2,],100*errores[3,],
                  100*errores[4,]),pch=19, cex=1,
        col=c("black","blue", "lightblue","red"),
        type="b",ylab="%Errores", xlab="errores:F, M, global")

legend("topleft", legend=c("Aparente","CV1","CV2","Test"),
       pch=19, cex=.6, col=c("black","blue", "lightblue","red"))

print(100*errores, digits=4)
#                 No     Yes   Global
# Aparentes      10.08  14.02  11.86
# CVdelta1         NA    NA    11.43
# Test B=1000    13.18  18.19  15.47





###################### SUPPORT VECTOR MACHINE ########################
## 1. Ajuste de modelos con distintos tipos de kernel

# a. Kernel lineal
(svmfit = svm(CAD~., data=cad1 , kernel ="linear", scale = FALSE ));  summary(svmfit)

ypred = predict(svmfit,cad1)      # Predicciones
tab <-table(CAD,ypred); tab       # Matriz de confusiÃ³n
(1-sum(diag(tab))/sum(tab))* 100  # Tasa de error global
((tab[1,2]/sum(tab[1,])))*100     # Tasa de error grupo 0
((tab[2,1]/sum(tab[2,])))*100     # Tasa de error grupo 1

# b. Kernel polinomial 
svmfit2 =svm(CAD~., data=cad1, kernel="polynomial", cost=1, degree=2); summary(svmfit2)

ypred2 =predict(svmfit2,cad1)      # Predicciones
tab2<-table(CAD,ypred2); tab2      # Matriz de confusiÃ³n
(1-sum(diag(tab2))/sum(tab2))* 100 # Tasa de error global
((tab2[1,2]/sum(tab2[1,])))*100    # Tasa de error grupo 0
((tab2[2,1]/sum(tab2[2,])))*100    # Tasa de error grupo 1

# c. Kernel radial 

svmfit3 =svm(CAD~., data=cad1, kernel="radial", gamma =1, cost =1); summary(svmfit3)

ypred3 = predict(svmfit3,cad1)     # Predicciones
tab3 <- table(CAD,ypred3); tab3    # Matriz de confusiÃ³n
(1-sum(diag(tab3))/sum(tab3))* 100 # Tasa de error global
(tab3[1,2]/sum(tab3[1,]))*100      # Tasa de error grupo 0
(tab3[2,1]/sum(tab3[2,]))*100      # Tasa de error grupo 1


### Resumen de tasas de error de clasificaciÃ³n para cada modelo (distinto tipo de kernel)
### sin haber calibrado los parÃ¡metros cost y gamma.
models0 = list(svmfit,svmfit2,svmfit3)

pred0 = sapply(models0,function(mod){predict(mod,cad1)})

tasas_ap0 = sapply(1:3,function(i){tasas_error(CAD,pred0[,i])})
table0 = as.data.frame(t(tasas_ap0))
rownames(table0) = c('Lineal','Polinomial','Radial')
colnames(table0) = c("Global","Grupo 0","Grupo 1")
round(table0,2)



## 2. Ajuste de modelos con distintos tipos de kernel y parÃ¡metros calibrados
## Uso de la funciÃ³n tune, hay que definir mÃºltiple valores de costo (por violaciÃ³n
## en las restricciones)

set.seed(1999) # La funiÃ³n tuning validad los error por 10 fold CV, por lo que es necesario
# fijar una semilla para poder replicar resultados

mlineal = tune(svm ,CAD~., data=cad1 , kernel ="linear",
               ranges = list(cost=c(1:3,2^(2:9)))); summary(mlineal)
## Mejor cost=16, menor error=0.1156

mpol = tune(svm ,CAD~., data=cad1 , kernel ="polynomial",
            ranges = list(cost=c(1:5), degree = c(2,3,4)))
summary(mpol)
## Mejor cost=5, menor error= 0.1313

mradial = tune(svm , CAD~., data = cad1, kernel ="radial",
               ranges = list(cost=c(1:3, 2^(2:9)),gamma =c(0.5 ,1 ,2 ,3 ,4) ))
summary(mradial)
## Mejor cost=2, menor error=0.1476, gamma=0.5

# Mejores modelos
mlineal$best.model
mpol$best.model
mradial$best.model

# Mejores parametros
mlineal$best.parameters
mpol$best.parameters
mradial$best.parameters

# Tasas aparentes de los mejores modelos. 
models = list(mlineal$best.model,mpol$best.model,mradial$best.model)

pred = sapply(models,function(mod){predict(mod,cad1)})
tasas_ap = sapply(1:3,function(i){tasas_error(CAD,pred[,i])})
table1 = as.data.frame(t(tasas_ap))
rownames(table1) = c('Lineal','Polinomial','Radial')
colnames(table1) = c("Global","Grupo 0","Grupo 1")
round(table1,2)

# Tenemos que el mejor modelo es el radial con 
# cost = 2 y gamma = 0.5
mradial$best.parameters

############## VALIDACIÃN DE TASAS DE ERROR POR MÃTODOS DE REMUESTREO    ##################

ftrain <- function(train,test = T){
  mtemp =tune(svm , CAD~., data = cad1[train,], kernel ="radial",
              ranges = list(cost=c(1:3, 2^(2:5)),gamma =c(0.5 ,1 ,2 ,3 ,4) ))
  if(test == F){
    return(tasas_error(CAD,predict(mtemp$best.model,newdata = cad1[,-14])))
  }
  return(tasas_error(CAD[-train],predict(mtemp$best.model,newdata = cad1[-train,-14])))
}

### ValidaciÃ³n de tasas de error por 10-fold Cross Validation, B=200
cvk <- function(k){
  if(k == 235){
    tasas_cv = sapply(1:k,function(i){ftrain(c(1:235)[-i])})
    return(tasas_cv)
  }
  index = sample(1:235,235)
  folds = cut(1:235, breaks = k, labels = F)
  tasas_cv = sapply(1:k,function(i){ftrain(-index[folds == i])})
  tasas_cv[is.na(tasas_cv)] = 0
  return(tasas_cv)
}

cvkn <- function(B,k){
  temp <-  replicate(B, apply(cvk(k), 1, function(x){mean(x[!is.nan(x)])}))
  return(temp)
}

t0=proc.time()
CV <- cvkn(200,10)
(proc.time()-t0)
#     user   system  elapsed 
# 28201.02    21.61 29037.12 

table3 <-apply(CV,1,mean)
#       No   Yes 
#15.58 11.62 20.04 

####  Repeated holdout con B=500
reptt <- function(B,p){
  return(apply(replicate(B, sample(1:235, 189*p)),2,ftrain))
}
set.seed(1)
t0=proc.time()
rep <- reptt(500,4/5)
(proc.time()-t0)
#    user  system elapsed 
# 6426.04   14.15 8511.35 

table1<-apply(rep,1,mean)
#       No   Yes 
#16.29 12.15 20.93 

### Resultados: Tasas de error

# |                |   ParÃ¡metros sin calibrar  |   ParÃ¡metros calibrados   |
# |                | Global | Grupo 0 | Grupo 1  | Global | Grupo 0 | Grupo 1
# ---------------------------------------------- |----------------------------| 
# |SVM Lineal      | 9.32   |  6.98   |  12.15   |  9.75  |   5.43  |  14.95  |
# |SVM Polinomial  | 3.39   |  3.88   |   2.80   |  9.32  |   7.75  |  11.21  |
# |SVM Radial      | 3.39   |  3.88   |   2.80   |  3.39  |   3.88  |   2.80  |


### Resultados: Tasas de error validadas para SVM radial a travÃ©s de mÃ©todos de 
### remuestreo

# |                                | Global | Grupo 0 | Grupo 1  | 
# -------------------------------------------------------------- |
# | CV K=10, B=200                 | 15.58  | 11.62   |  20.04   |
# | Repeated holdout (B=50, 4/5)   | 16.29  | 12.15   |  20.93   |





##### Ejercicio 3 ####
library(nnet)
glucose <- read.csv("Glucose1.txt")
#--Exploración de los datos -------
str(glucose)
glucose <- glucose[,-1]
glucose$Class <- as.factor(glucose$Class)
for (i in 1:5) {glucose[,i]<- as.numeric(glucose[,i])}
xtable(cor(glucose[,1:5]))
par(mfrow=c(2,3))
for (i in 1:5) {
  boxplot(glucose[,i]~ Class,data = glucose, ylab = colnames(glucose)[i],border=colores,
          main=colnames(glucose)[i]) 
}
levels(glucose$Class) <- c(3,1,2)
par(mfrow=c(1,1))
pairs(x = glucose[, 1:5],
      col = colores[1:3][glucose$Class], pch = 19)
pca3 <- prcomp(glucose[,-6],scale=TRUE)
plot(predict(pca3)[,1:2], col = colores[glucose$Class],pch=19,cex=1.5, main="Componentes principales")
#------Modelo 1------
mod_rl <- multinom(Class~.,data = glucose,maxit =1000,trace=F)
summary(mod_rl)
errores=matrix(NA, ncol=4, nrow=6)
colnames(errores)=c("3", "1","2", "Global")
rownames(errores)=c("Aparentes M1","Test M1","Aparentes M2","Test M2",
                    "Aparentes M3","Test M3") 
errores
#Errores aparentes
tab=table(glucose$Class,predict(mod_rl, glucose, type = "class"))
for(i in 1:3){
  errores[1,i]=1- tab[i,i]/sum(tab[i,])
}
errores[1,4] <- 1-sum(diag(tab))/sum(tab)
errores
#Training-test
set.seed(16)
B = 1000
training_3 <- replicate(B, sample(1:145,116)) #cada columna es un conjunto entrenamiento
err <- matrix(NA,nrow = B,ncol = 4); colnames(err)<- c("3", "1","2","Global")
for (i in 1:B) {
  mod_ <- multinom(Class~., data=glucose[training_3[,i],], maxit =1000,trace=F)
  tab=table(glucose$Class[-training_3[,i]],predict(mod_, glucose[-training_3[,i],], type = "class"))
  for(j in 1:3){
    err[i,j]=1- tab[j,j]/sum(tab[j,])
  }
  err[i,4] <- 1-sum(diag(tab))/sum(tab)
}
(errores[2,] <- colMeans(err))
errores
#--------Modelo 2----------
mod_rl2 <- multinom(Class~.+ Fglucose:GlucoseInt,data = glucose,maxit =1000,trace=F)
summary(mod_rl2)
#Errores aparentes
tab2=table(glucose$Class,predict(mod_rl2, glucose, type = "class"))
for(i in 1:3){
  errores[3,i]=1- tab2[i,i]/sum(tab2[i,])
}
errores[3,4] <- 1-sum(diag(tab2))/sum(tab2)
errores
#Training-test
set.seed(16)
err2 <- matrix(NA,nrow = B,ncol = 4); colnames(err)<- c("3", "1","2","Global")
for (i in 1:B) {
  mod_2 <- multinom(Class~.+Fglucose:GlucoseInt, data=glucose[training_3[,i],], maxit =1000,trace=F)
  tab2=table(glucose$Class[-training_3[,i]],predict(mod_2, glucose[-training_3[,i],], type = "class"))
  for(j in 1:3){
    err2[i,j]=1- tab2[j,j]/sum(tab2[j,])
  }
  err2[i,4] <- 1-sum(diag(tab2))/sum(tab2)
}
(errores[4,] <- colMeans(err2))
errores
#------ Modelo 3 --------
mod_rl3 <- multinom(Class~.+ Fglucose:GlucoseInt+GlucoseInt:InsulineResist,data = glucose,maxit =1000,trace=F)
summary(mod_rl3)
#Errores aparentes
tab3=table(glucose$Class,predict(mod_rl3, glucose, type = "class"))
for(i in 1:3){
  errores[5,i]=1- tab3[i,i]/sum(tab3[i,])
}
errores[5,4] <- 1-sum(diag(tab3))/sum(tab3)
errores
#Training-test
set.seed(16)
err3 <- matrix(NA,nrow = B,ncol = 4); colnames(err3)<- c("3", "1","2","Global")
for (i in 1:B) {
  mod_3 <- multinom(Class~.+Fglucose:GlucoseInt+GlucoseInt:InsulineResist, data=glucose[training_3[,i],], maxit =1000,trace=F)
  tab3=table(glucose$Class[-training_3[,i]],predict(mod_3, glucose[-training_3[,i],], type = "class"))
  for(j in 1:3){
    err3[i,j]=1- tab3[j,j]/sum(tab3[j,])
  }
  err3[i,4] <- 1-sum(diag(tab3))/sum(tab3)
}
(errores[6,] <- colMeans(err3))
errores <- errores*100