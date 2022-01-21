### TAREA 5 ### SML
#------- Bagging y Random forest
options(digits = 2)
library(rpart)
library(rpart.plot)
library(randomForest)
library(caret)

#------------Ejercio 2----------
library(MASS)
library(nnet)
col = c(
  "turquoise",
  "salmon",
  "navyblue",
  "darkorange1",
  'yellowgreen',
  'gray54',
  "red",
  "green",
  "purple"
)
# Función para calcular las tasas de error de clasificación global y
# por categoría.
te <- function(y, pred) {
  tab <- table(y, pred)
  r <- rep(NA, 7)
  for (i in 1:6) {
    r[i + 1] <- 1 - tab[i, i] / sum(tab[i, ])
  }
  r[1] <- 1 - sum(diag(tab)) / sum(tab)
  return(r * 100)
}

# Análisis exploratorio
data(fgl)
str(fgl)
? fgl
attach(fgl)
cor(fgl[, 1:9])
par(mfrow = c(2, 3))
for (i in 1:9) {
  boxplot(fgl[, i] ~ type, col = col, main = colnames(fgl)[i])
}
table(type)
# Ajuste del árbol
? rpart
set.seed(1220)
tree1 <- rpart(type ~ ., data = fgl, method = "class")
summary(tree1)
# Gráfica del árbol de clasificación
par(mfrow = c(1, 1))
plot(tree1, col = col[1])
text(tree1)
rpart.plot(tree1)
plotcp(tree1)
# Con menor complejidad
tree2 <-
  prune(tree1, cp = tree1$cptable[which.min(tree1$cptable[, "xerror"]), "CP"])
summary(tree2)
plot(tree2, col = col[1])
text(tree2)
rpart.plot(tree2)
te(type, predict(tree2, fgl, type = "class"))
# Predicciones
pred <- predict(tree1, type = "class")
table(pred, type)
errores = matrix(NA, ncol = 7, nrow = 7)
colnames(errores) = c("Global", levels(type))
rownames(errores) = c(
  "Aparentes Tree",
  "Aparentes RL",
  "Test RL",
  "Aparentes Baggin",
  "oob-Bagging",
  "Aparentes RandF",
  "oob-RandF"
)
errores
errores[1, ] <- te(type, pred)
# ----- Ajuste del modelo de regresión logística multinomial
mod_rl <- multinom(type ~ .,
                   data = fgl,
                   maxit = 1000,
                   trace = F)
summary(mod_rl)
confint(mod_rl)
#Errores aparentes
errores[2, ] <- te(type, predict(mod_rl, fgl, type = "class"))
errores
#Training-test
set.seed(16)
B = 1000
training <-
  replicate(B, createDataPartition(type, p = 0.8)$Resample1) #cada columna es un conjunto entrenamiento
err <-
  matrix(NA, nrow = B, ncol = 7)
colnames(err) <- colnames(errores)
for (i in 1:B) {
  mod_ <-
    multinom(
      type ~ .,
      data = fgl,
      maxit = 1000,
      trace = F,
      subset = training[, i]
    )
  err[i, ] <-
    te(type[-training[, i]], predict(mod_, fgl[-training[, i], ], type = "class"))
}
errores[3, ] <- colMeans(err, na.rm = TRUE)
errores
# -----Bagging
set.seed(3)
tree.bg = randomForest(
  type ~ .,
  data = fgl,
  ntree = 1000,
  mtry = 9,
  importance = TRUE
)
plot(tree.bg)
tree.bg$importance
importance(tree.bg)
varImpPlot(tree.bg, pch = 19)
# Error aparente
(errores[4, ] = te(type, predict(tree.bg, fgl)))
table(type, predict(tree.bg, fgl))
# Errores de clasificación en el las observaciones oob.
(errores[5, ] <- te(type, tree.bg$predicted))
tree.bg$confusion
errores

# -----Random Forest
set.seed(128)
randf = randomForest(
  type ~ .,
  data = fgl,
  importance = T,
  ntree = 1000,
  mtry = 3
)
importance(randf)
varImpPlot(randf, pch = 19)
# Error aparente
(errores[6, ] <- te(type, predict(randf, fgl)))

# Errores de clasificación en las observaciones oob
(errores[7, ] <- te(type, randf$predicted))
randf$confusion
errores
# Cambiando mtry
fits <- list()
err_mtry <- matrix(NA, nrow = 9, ncol = 7)
colnames(err_mtry) <- colnames(errores)
rownames(err_mtry) <- 1:9
t = proc.time()
set.seed(128)
for (mtry in 1:9) {
  fits[[mtry]] <-
    randomForest(type ~ .,
                 data = fgl,
                 mtry = mtry,
                 ntree = 1000)
  err_mtry[mtry, ] = te(type, fits[[mtry]]$predicted) #predicted oob error ~Test error
}
proc.time() - t
err_mtry
# Graficas para determinar el número de árboles con el que se
#estabiliza el error
par(mfrow = c(1, 1))
plot(
  1:1000,
  fits[[1]]$err.rate[, 2] * 100,
  type = "l",
  xlab = "ntrees",
  ylab = "Tasa de error (%OOB)",
  col = col[1],
  frame.plot = F
)
for (i in 2:9) {
  lines(1:1000,
        fits[[i]]$err.rate[, 2] * 100,
        type = "l",
        col = col[i])
}
legend(
  "topleft",
  legend = c("k = 1", "k = 2", "k = 3", "k = 4"),
  col = col[1:4],
  lty = 1,
  horiz = T
)
legend(
  "center",
  legend = c("k=5", "k=6", "k=7", "k=8", "k=9"),
  col = col[5:9],
  lty = 1,
  horiz = T
)
