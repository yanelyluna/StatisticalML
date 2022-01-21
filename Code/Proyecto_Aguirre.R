# APRENDIZAJE ESTAD?STICO AUTOMATIZADO
#          PROYECTO FINAL
# Equipo:
#Aguirre Armada Guillermo
#Figueroa Torres Ivan Emiliano
#Luna Guti?rrez Yanely
#Ortiz Silva Ana Beatriz
#
# Objetivo: Clasificaci?n de diez d?gitos del 0 al 9 escritos a mano por estudiantes de 
# bachillerato y empleados de la Oficina de Censos en EUA mediante Deep Neural Networks y 
# Regresión logística regularizada y no regularizada.
# En este laboratorio se ajustarán redes neuronales (1) cambiando input_dropout_rate y (2)
# utilizando random grid search

# Paqueter?as------
options(width = 150, digits = 4)
rm(list = ls())
library(nnet)
library(glmnet)
library(h2o)
h2o.init(nthreads = -1)  #internet connection required.
h2o.removeAll()
#Bases de datos--------
setwd("C:\\Users\\Yanely\\Documents\\2021-1\\SML")
train <- h2o.uploadFile("MNISTtrain.csv")
test <- h2o.uploadFile("MNISTtest.csv")
validate <- h2o.uploadFile("MNISTvalidate.csv")

y <- "C785"  #response column: digits 0-9
x <- setdiff(names(train), y)  #vector of predictor column names

train[, y] <- as.factor(train[, y])
test[, y]  <- as.factor(test[, y])


#Display the first nine observations,and for each one print
#its class membership.Each obs. is defined by 28x28=784 pixels
par(mfrow = c(3, 3), mai = c(.2, .2, .2, .2))
for (i in 1:9) {
  y_1 = as.matrix(train[i, 1:784])
  dim(y_1) = c(28, 28)
  image(y_1[, nrow(y_1):1], axes = F,
        col = gray(255:0 / 255))
  text(0.2,
       0,
       train[i, 785],
       cex = 3,
       col = 2,
       pos = c(3, 4))
}
par(mfrow = c(1, 1), mai = c(1, 1, 1, 1))
#-------------DNN----------------------------------------------
table <- matrix(nrow = 18,ncol =11) # Tabla resumen
table2 <- matrix(nrow = 4,ncol =11)
table3 <- matrix(nrow = 5,ncol =11)

# Ajuste de primera red neuronal, red sencilla

# Lista de parámetros a considerar:
# activation - función de activación para relacionar los nodos de cada capa (tanh, rectifier
# etc.)
# epochs - número de veces que se itera (secuencia) el conjunto de datos
# distribution - distribución de la variable respuesta (en este caso, la variable toma
# valores entre 0 y 9, por lo que la distribución será multinomial)
# l1 - valor de lambda de regularización para añadir estabilidad y mejorar la
# generalización; establece el valor de muchos pesos en 0 (predeterminado)
# input_dropout_ratio - especifica la relación de abandono de la capa de entrada para
# mejorar la generalización. Los valores sugeridos son 0.1 o 0.2. 

system.time(
  dl1 <- h2o.deeplearning(x = x, y = y,
                          training_frame = train,
                          model_id = "dl1",
                          hidden = c(100,100,100),
                          activation = "RectifierWithDropout",
                          epochs = 20, 
                          distribution = "multinomial",
                          l1 = 1e-3, 
                          input_dropout_ratio = 0.2,
                          nfolds = 5,
                          seed = 1))
# user  system elapsed 
# 14.06    7.13 1443.69 
#ulti
# user  system elapsed 
# 6.65    1.64 2297.42
(dl1)
# Estadísticas, matrices de confusión y tasas de error sobre conjuntos de entrenamiento
# y prueba por grupo y globales
(pftr <- h2o.performance(dl1,newdata = train))
(pfte <- h2o.performance(dl1,newdata = test))

par(pch = 16, cex.axis = 1.0, cex.lab = 1.0,cex=.6)
par(mfrow=c(1,2))
plot(dl1, metric="logloss") # Logloss sobre conjunto de entrenamiento
plot(dl1, metric="classification_error") # Tasa de error de clasificación global sobre
# conjunto de entrenamiento

# Funcion de informacion para tabla resumen

info <-function(mod,ptrain,ptest){
  return(
    c(ptrain@metrics$cm$table$Error[11], # Error global conjunto entrenamiento
      ptest@metrics$cm$table$Error[11],  # Error global conjunto de prueba
      mod@model$cross_validation_metrics@metrics$cm$table$Error[11],       # Error CV
      mod@model$run_time/60000,  # Tiempo de ajuste de la red 
      paste(mod@allparameters$hidden, collapse="/"), # Capas y número de nodos en ellas
      mod@allparameters$epochs, # Número de épocas
      mod@allparameters$l1, # Regulización(lambda)
      mod@allparameters$input_dropout_ratio, # Ratio 
      ptrain@metrics$logloss, # Logloss conjunto entrenamiento
      ptest@metrics$logloss,  # Logloss conjunto prueba
      mod@model$cross_validation_metrics@metrics$logloss  # Logloss CV
    ))
}

(table[1,] = info(dl1,pftr,pfte))

save.image('Lab19_Deeplearning_MNISTv2.Rdata')

#################################
# Ajuste de red neuronal a través de un grid aleatorio. En el laboratorio 18 se ajustó
# un grid, pero este no era aleatorio. 
# Para el grid se tomarán en cuenta los siguientes hiperparámetros: hidden (número de
# capas y nodos), epochs, y l1 (lambda, regularización)

# Asignamos número de capas y nodos en las mismas de manera aleatoria. Se 
# obtienen 100 conjuntos
(hidden_opt = lapply(1:100, function(x)10+sample(150,sample(5), replace=TRUE)))


(hidden_opt2 = lapply(1:100, function(x)10+sample(1500,sample(5), replace=TRUE)))

# Secuencia para los valores de lambda (será el equivalente a una malla de valores)
(l1_opt = seq(1e-5,1e-3,1e-5)) 

# Número de épocas (será equivalente a una malla de valores)
(epoch_opt=seq(20,40,1))
(epoch_opt2=seq(10,40,1))

hyper_params <- list(hidden = hidden_opt, l1 = l1_opt, epochs=epoch_opt)
hyper_params2 <- list(hidden = hidden_opt2, l1 = l1_opt, epochs=epoch_opt2)

search_criteria = list(strategy = "RandomDiscrete",
                       max_models = 9,
                       seed=16)
search_criteria1 = list(strategy = "RandomDiscrete",
                        max_models = 4,
                        seed=1999)

search_criteria2 = list(strategy = "RandomDiscrete",
                        max_models = 4,
                        seed=9)
# score_interval - especifica el intervalo de tiempo más corto (en segundos)
# que se debe esperar entre la puntuación del modelo
system.time(
  randomg1 <- h2o.grid("deeplearning",
                       grid_id = "randomg1",
                       hyper_params = hyper_params,
                       search_criteria = search_criteria,
                       x = x,
                       y = y,
                       distribution = "multinomial",
                       training_frame = train,
                       seed = 1,
                       score_interval = 2,
                       stopping_rounds = 3,
                       stopping_tolerance = 0.05,
                       stopping_metric = "misclassification"))
system.time(
  randomg_1 <- h2o.grid("deeplearning",
                        grid_id = "randomg_1",
                        hyper_params = hyper_params,
                        search_criteria = search_criteria1,
                        x = x,
                        y = y,
                        distribution = "multinomial",
                        training_frame = train,
                        seed = 1,
                        score_interval = 2,
                        stopping_rounds = 3,
                        stopping_tolerance = 0.05,
                        stopping_metric = "misclassification"))
system.time(
  randomg_1 <- h2o.grid("deeplearning",
                        grid_id = "randomg_1",
                        hyper_params = hyper_params,
                        search_criteria = search_criteria1,
                        x = x,
                        y = y,
                        distribution = "multinomial",
                        training_frame = train,
                        seed = 1,
                        score_interval = 2,
                        stopping_rounds = 3,
                        stopping_tolerance = 0.05,
                        stopping_metric = "misclassification"))

system.time(
  randomg_2 <- h2o.grid("deeplearning",
                        grid_id = "randomg_2",
                        hyper_params = hyper_params2,
                        search_criteria = search_criteria2,
                        x = x,
                        y = y,
                        distribution = "multinomial",
                        training_frame = train,
                        seed = 1,
                        score_interval = 2,
                        stopping_rounds = 3,
                        stopping_tolerance = 0.05,
                        stopping_metric = "misclassification"))

# ultimo
# user   system  elapsed 
# 30.44     7.93 16051.69 
#randomg1
# user   system  elapsed 
# 26.97     6.72 12896.94 
summary(randomg1) # Resumen 
summary(randomg_1)
# user  system elapsed 
# 15.55    4.91 7410.44
modelos <- lapply(randomg1@model_ids, h2o.getModel) # Modelos obtenidos con el random grid
modelo_save<-sapply(1:18,function(i) h2o.saveModel(object=modelos[[i]], path=getwd(),force=TRUE))
model9<-h2o.saveModel(object=modelos[[10]], path=getwd(),force=TRUE)
print(model_path0)
modelo_bueno<- h2o.loadModel(modelo_save[[5]])

modelos2 <- lapply(randomg_1@model_ids, h2o.getModel) # Modelos obtenidos con el random grid

# Tasa de error sobre conjunto de entrenamiento para cada una de las redes ajustadas
par(mfrow = c(3,3), mai = c(.5,.5,.5,.5))
for(i in 1:18){
  plot(modelos[[i]], metric = "classification_error",cex=.5)
}

par(mfrow = c(2,2), mai = c(.5,.5,.5,.5))
for(i in 1:4){
  plot(modelos2[[i]], metric = "classification_error",cex=.5)
}

# Performance train|test. Estadísticas, matriz de confusión y tasas de error en 
# conjunto de entrenamiento y prueba
pertrain <- lapply(modelos,h2o.performance, newdata = train)
pertest <-  lapply(modelos,h2o.performance, newdata = test)

pertrain2 <- lapply(modelos2,h2o.performance, newdata = train)
pertest2 <-  lapply(modelos2,h2o.performance, newdata = test)
# Obtenemos información de cada una de las redes ajustadas
for(i in 1:17){
  table[i+1,] = info(modelos[[i]],pertrain[[i]],pertest[[i]])
}
for(i in 1:4){
  table2[i,] = info(modelos2[[i]],pertrain2[[i]],pertest2[[i]])
}
table
# Estructura al data set
table = as.data.frame(table)
table[,c(1:4,6:11)] = apply(table[,c(1:4,6:11)],2,as.numeric)
colnames(table) = c('Train','Test','CV','Run time','Hidden','Epochs','l1','dropout_ratio','Logloss','Logloss','Logloss')
rownames(table) = c('dl1',sapply(1:17,function(i) paste('Grid',as.character(i))))

print(100*table[,c(1:3)],digits = 3)
####### bueno #####
# 
#         Train Test   CV
# dl1     4.014 4.96 5.53
# Grid 1  2.949 4.76 4.69
# Grid 2  3.217 4.39 5.39
# Grid 3  2.884 4.42 5.21
### Grid 4  0.872 3.47 4.34
# Grid 5  4.507 5.90 5.59
# Grid 6  2.690 4.79 5.10
# Grid 7  3.711 5.35 5.38
# Grid 8  3.755 5.24 5.40
# Grid 9  4.024 5.83 5.58
# Grid 10 4.263 5.79 5.67
# Grid 11 2.754 4.90 5.37
# Grid 12 4.308 5.37 6.36
# Grid 13 4.149 5.00 6.45
# Grid 14 2.595 4.76 5.86
# Grid 15 2.998 5.35 5.40
# Grid 16 2.590 4.68 5.48
# Grid 17 4.129 6.32 6.77

table2 = as.data.frame(table2)
table2[,c(1:4,6:11)] = apply(table2[,c(1:4,6:11)],2,as.numeric)
colnames(table2) = c('Train','Test','CV','Run time','Hidden','Epochs','l1','dropout_ratio','Logloss','Logloss','Logloss')
rownames(table2) = c('dl1',sapply(1:9,function(i) paste('Grid',as.character(i))))
print(100*table2[,c(1:3)],digits = 3)
print(table[,c(4:11)],digits = 4)
print(table2[,c(4:11)],digits = 4)
par(mfrow = c(1,1), mai = c(1,1,1,1))
(reord = sort(table[2:10,10], index.return=TRUE)$ix)
table[2:10,] =  table[reord+1,]

print(table[,c(4:11)],digits = 4)
########   modelos  ####
#         Run time             Hidden Epochs      l1 dropout_ratio Logloss Logloss.1 Logloss.2
# dl1        7.380        100/100/100 20.042 0.00100           0.2 0.13473    0.1615    0.1895
# Grid 1     7.328     92/151/131/134  6.461 0.00046           0.0 0.09492    0.1606    0.1673
# Grid 2     7.353     99/101/143/152  5.974 0.00076           0.0 0.10616    0.1467    0.1845
# Grid 3     4.352   62/81/57/121/102  5.887 0.00059           0.0 0.09427    0.1501    0.1849
# Grid 4     7.973    97/74/94/144/62  8.387 0.00012           0.0 0.02679    0.1443    0.1862
# Grid 5     6.382      129/157/73/76  4.573 0.00068           0.0 0.14024    0.1843    0.1877
# Grid 6     4.529             105/57  4.927 0.00061           0.0 0.09764    0.1595    0.1906
# Grid 7     4.567 32/103/122/100/145  7.282 0.00066           0.0 0.12146    0.1707    0.1909
# Grid 8     4.675         100/49/120  4.849 0.00076           0.0 0.12529    0.1730    0.1961
# Grid 9     6.911    155/18/78/18/52  5.016 0.00066           0.0 0.13420    0.1895    0.1961
# Grid 10    6.497  115/104/117/80/70  5.056 0.00068           0.0 0.13580    0.1763    0.1963
# Grid 11    3.328           48/70/11  8.413 0.00061           0.0 0.09513    0.1784    0.2102
# Grid 12    7.512     148/133/127/66  4.281 0.00100           0.0 0.14103    0.1690    0.2190
# Grid 13    5.248         146/33/137  3.738 0.00100           0.0 0.13866    0.1695    0.2258
# Grid 14    2.484                 37  8.643 0.00059           0.0 0.09367    0.1717    0.2287
# Grid 15    5.747                118  6.348 0.00046           0.0 0.10286    0.1943    0.2322
# Grid 16    4.024                 88  5.887 0.00060           0.0 0.08998    0.1590    0.2354
# Grid 17    2.410       11/154/55/74 11.019 0.00060           0.0 0.13783    0.2120    0.2467

windows()
matplot(1:18, cbind(table[,1], table[,2],table[,3], table[,9], table[,10],table[,11]),pch=19,cex=1.0, 
        col=c("yellow","orange", "red","lightblue","blue","darkblue"),
        type="b",ylab="logloss/Errors",xlab = "Modelos")
legend("topright", legend=c("Error_train", "Error_test", "Error_CV",
                              "logloss_train", "logloss_test","logloss_CV"), pch=19, cex=.6,
       col=c("yellow","orange", "red","lightblue","blue","darkblue"))

modelos[[5]]

predDNN <- h2o.predict(modelos[[5]],validate)$predict
h2o.exportFile(predDNN, "Proyecto_Aguirre_DNN_pred.csv")

# ----------Modelo de regresi?n log?stica No regularizado------
info <- function(mod,ptr,pte){
  return(c(
    ptr@metrics$cm$table$Error[11], # Error globlal en conjunto de entrenamiento
    pte@metrics$cm$table$Error[11], # Error globlal en conjunto de prueba
    mod@model$cross_validation_metrics@metrics$cm$table$Error[11], # Error CV
    ptr@metrics$logloss, # Logloss en conjunto de entrenamiento
    ptr@metrics$residual_deviance, # Devianza en conjunto de entrenamiento
    pte@metrics$logloss, # Logloss en conjunto de prueba
    pte@metrics$residual_deviance, # Devianza en conjunto de entrenamiento
    mod@model$lambda_best # Mejor lambda (en caso de regularizaci?n)
  ))
}



# Regresion Logistica Multinomial sin regularizaci?n 

modelo = list()
pftrain = list()
pftest = list()

infotable <- matrix(ncol = 8,nrow = 2)
colnames(infotable) = c('Train','Test','CV','Logloss', 'R Deviance','Logloss', 'R Deviance','l')
rownames(infotable) = c('RegLogNoR', 'RegLognoR k=5')

# Ajuste del modelo de regresi?n log?stica sin regularizaci?n
#Lambda y n-folds= 0
system.time(
  modelo[[1]] <- h2o.glm(x=x,y=y,
                          training_frame=train,
                          nfolds=0, 
                          family="multinomial",
                          lambda=0
  ))
# user  system elapsed 
# 0.87    0.08   22.11 


(summary(modelo[[1]])) 
coe <- h2o.coef(modelo[[1]]) 

# Performance. Desempe?o sobre el conjunto de entrenamiento y prueba del modelo
(pftrain[[1]] <- h2o.performance(modelo[[1]],train))
(pftest[[1]] <- h2o.performance(modelo[[1]],test))
windows()
h2o.std_coef_plot(modelo[[1]]) # Regresi?n log?stica


(infotable[1,c(1:2,4:8)] = info(modelo[[1]],pftrain[[1]],pftest[[1]]))
infotable
# Regresion Logistica Multinomial sin regularizaci?n validada por 10-fold Cross Validation

system.time(
  modelo[[2]] <- h2o.glm(x=x,y=y,
                          training_frame=train,
                          nfolds=10, # se incluye el n?mero de folds para la validaci?n
                          family="multinomial",
                          lambda=0))

# user  system elapsed 
#   1.85    2.17  995.21 

# Performance
(pftrain[[2]] <- h2o.performance(modelo[[2]],train))
(pftest[[2]] <- h2o.performance(modelo[[2]],test))

h2o.std_coef_plot(modelo[[2]]) # coeficientes obtenidos por CV

(infotable[2,] = info(modelo[[2]],pftrain[[2]],pftest[[2]]))
infotable

(pred_rnr <- h2o.predict(modelo[[2]],validate)$predict) 
h2o.exportFile(pred_rnr, "Proyecto_Aguirre_RegNoLogReg_pred.csv")
###Sacamos los errores
#Construimos la matriz
err.No.R <- matrix(NA, nrow = 11, ncol = 3)
rownames(err.No.R) <- c("Global", 1:10)
colnames(err.No.R) <- c("Train", "Test", "C-Validation")
err.No.R
##########################
modelo[[2]]@model$training_metrics@metrics$cm$table
error_train <-
  modelo[[2]]@model$training_metrics@metrics$cm$table$Error
err.No.R[, 1] <- c(error_train[11], error_train[1:10])


pf_test <- h2o.performance(modelo[[2]], test)
pf_test@metrics$cm$table
error_test <- pf_test@metrics$cm$table$Error
err.No.R[, 2] <- c(error_test[11], error_test[1:10])

modelo[[2]]@model$training_metrics@metrics$cm$table
error_cv <-
  modelo[[2]]@model$training_metrics@metrics$cm$table$Error
err.No.R[, 3] <- c(error_cv[11], error_cv[1:10])

err.No.R*100


# ----------Modelo de regresi?n log?stica regularizado---------
# Matriz de tasas de error de clasificaci?n
errores <- matrix(NA, nrow = 11, ncol = 3)
rownames(errores) <- c("Global", 1:10)
colnames(errores) <- c("Train", "Test", "Cross-Validation")
errores

set.seed(1234)
t = proc.time()
modelo_RLogM <- h2o.glm(
  family = "multinomial",
  x = x,
  y = y,
  training_frame = train,
  alpha = 0,
  lambda_search = TRUE
)
proc.time() - t
# user  system elapsed
# 2.0     0.4   102.7
path <- "C:\\Users\\Yanely\\Documents\\2021-1\\SML"
mojo_destination <-
  h2o.download_mojo(model = modelo_RLogM, path = path)

# Modelo con validaci?n cruzada
set.seed(234)
t = proc.time()
#modelo_cv2 ridge
modelo_cv <-  h2o.glm(
  family = "multinomial",
  x = x,
  y = y,
  training_frame = train,
  alpha = 0,
  lambda_search = TRUE,
  nfolds = 10
)
proc.time() - t
mojo_destination <-
  h2o.download_mojo(model = modelo_cv, path = path)

(lambda_best <- modelo_RLogM@model$lambda_best) #0.0001831
(lambda_best_cv <- modelo_cv@model$lambda_best) #0.008276
# Tasas de error del modelo con train
modelo_RLogM@model$training_metrics@metrics$cm$table
error_train <-
  modelo_RLogM@model$training_metrics@metrics$cm$table$Error
errores[, "Train"] <- c(error_train[11], error_train[1:10])

#Tasas de error por validaci?n cruzada
modelo_cv@model$training_metrics@metrics$cm$table
error_cv <-
  modelo_cv@model$training_metrics@metrics$cm$table$Error
errores[, "Cross-Validation"] <- c(error_cv[11], error_cv[1:10])

#Performance en el conjunto test-----
perf_test <- h2o.performance(modelo_RLogM, test)
perf_test@metrics$cm$table
error_test <- perf@metrics$cm$table$Error
errores[, "Test"] <- c(error_test[11], error_test[1:10])
#         Train    Test Cross-Validation
#Global 0.04770 0.08856          0.06927
#1      0.01394 0.03194          0.02623
#2      0.01452 0.02652          0.02164
#3      0.06004 0.12662          0.09041
#4      0.07945 0.11757          0.09997
#5      0.04194 0.09159          0.05741
#6      0.06885 0.13196          0.10125
#7      0.02112 0.05059          0.03542
#8      0.04403 0.06490          0.06477
#9      0.07640 0.13656          0.11031
#10     0.06269 0.11803          0.09436

# Predicciones en el conjunto validate
pred_rlm <- h2o.predict(modelo_RLogM, validate)$predict
h2o.exportFile(pred_rlm, "Proyecto_Aguirre_RegLogReg_pred.csv")

#Gr?fica de la tasa de error de clasificaci?n por categor?a
plot(
  x = 1:11,
  y = 100 * error_test,
  type = "b",
  pch = 19,
  col = 4,
  main = "Tasas de error de clasificaci?n del Modelo RL Reg",
  ylab = "Tasa de error en %",
  xlab = "Categor?a"
)
lines(
  x = 1:11,
  y = 100 * error_cv,
  type = "b",
  pch = 19,
  col = 3
)
lines(
  x = 1:11,
  y = 100 * error_train,
  type = "b",
  pch = 19,
  col = 2
)
legend(
  "topleft",
  legend = c("Test", "CV(k=10)", "Train"),
  lty = 1,
  col = c(4, 3, 2),
  bty = "n",
  cex = 0.85
)
