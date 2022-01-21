## TAREA 6 - 
# Ejercicio 2
library(hdi); library(glmnet)
?riboflavin
data("riboflavin")
# Matrix model 
x = model.matrix(y ~ . , data = riboflavin) #para construir la matriz diseño
y = riboflavin$y #variable respuesta

str(x) #4088 variables y el intercepto

modelos <- list(3)
err=matrix(NA, ncol=6,nrow=3)
rownames(err)=c("ridge","lasso", "elastic net")
colnames(err)=c("TrnErr(Ap)",
                "  cv(k=10, B=1)", "cvsd",
                "  #betas(df)", "  lambda.min","  cv(k=10, B=100)")
err
Mcv <- list(3)
#Ridge, Lasso y elastic net
al <- c(0,1,0.5)

set.seed(1234)
t1=proc.time()
for (Nmod in 1:3){
  modelos[[Nmod]] <- glmnet(x,y,alpha = al[Nmod])
  Mcv[[Nmod]]=cv.glmnet(x, y,type.measure="mse",nfolds=10,alpha=al[Nmod])
  err[Nmod,1]=assess.glmnet(modelos[[Nmod]],s=Mcv[[Nmod]]$lambda.min,newx = x, newy = y)$mse #error aparente
  err[Nmod,2]=Mcv[[Nmod]]$cvm[which(Mcv[[Nmod]]$lambda==Mcv[[Nmod]]$lambda.min)] #error por cv 
                                                                        # del modelo con lambda min
  err[Nmod,3]=Mcv[[Nmod]]$cvsd[which(Mcv[[Nmod]]$lambda==Mcv[[Nmod]]$lambda.min)] #error estándar
  err[Nmod,4]=Mcv[[Nmod]]$nzero[which(Mcv[[Nmod]]$lambda==Mcv[[Nmod]]$lambda.min)]+1 #betas que no son cero
  err[Nmod,5]=Mcv[[Nmod]]$lambda.min #valor de lambda que min el mse
}
proc.time()-t1
err

coef(Mcv[[2]])

#Numero de betas distintas de cero y valor de lambda según el criterio
auxf <- function(i,crit){
  if(crit == "lambda.min"){
    a<-modelos[[i]]$beta[,which(modelos[[i]]$lambda==Mcv[[i]]$lambda.min)]
    a <- a[a!=0]
    b <- Mcv[[i]]$lambda.min
  }
  if(crit=="lambda.1se"){
    a<-modelos[[i]]$beta[,which(modelos[[i]]$lambda==Mcv[[i]]$lambda.1se)]
    a <- a[a!=0]
    b <- Mcv[[i]]$lambda.1se
  }
  if(crit=="df"){
    a<-modelos[[i]]$beta[,which(modelos[[i]]$df == max(modelos[[i]]$df))[1]]
    a <- a[a!=0]
    b <- modelos[[i]]$lambda[modelos[[i]]$df == max(modelos[[i]]$df)[1]][1]
  }
  return(c(length(a)+1,b))
}
(betas <- sapply(1:3,auxf,crit="lambda.min"))
(betas_lambda1se <- sapply(1:3,auxf,crit="lambda.1se"))
(betas_df <- sapply(1:3,auxf,crit="df"))
## GRÁFICAS
#Ridge
m <- c("ridge","lasso","elastic net")
auxf2 <- function(i,x){
  betas <- modelos[[i]]$beta[,which(modelos[[i]]$lambda==Mcv[[i]]$lambda.min)]
  betas <- betas[betas!=0]
  b<-betas[abs(betas)>x ]
  hist(betas, main=paste("Coeficientes",m[i]),col="#008080",breaks = 40)
  legend("topright",legend=paste(length(b),"abs(betas) mayores a",x))
  return(b)
}
#par(mfrow=c(1,1))
betasMayores <- list(3)
betasMayores <- lapply(1:3, auxf2,x=0.025)
betasMayores <- lapply(betasMayores, abs) 
betasMayores <- lapply(betasMayores, sort,decreasing = TRUE)
betasMayores[[1]][1:6]
betasMayores[[2]][1:6]
betasMayores[[3]][1:6]

plot(modelos[[1]])
par(mfrow=c(1,2))
plot(modelos[[1]], xvar = "lambda")
plot(Mcv[[1]])
#Lasso
plot(modelos[[2]], xvar = "lambda")
plot(Mcv[[2]])
#Elastic net 0.5
plot(modelos[[3]], xvar = "lambda")
plot(Mcv[[3]])

##Validación del error con CV B=100
B=100
Mcvaux <- list(3)
aux=matrix(NA,nrow=B, ncol=6)
set.seed(1234)
t1=proc.time()
for (Nmod in 1:3){
  for (Nrep in 1:B) {
    Mcvaux[[Nmod]]=cv.glmnet(x, y,type.measure="mse",nfolds=10,alpha=al[Nmod])
    aux[Nrep,Nmod]=Mcvaux[[Nmod]]$cvm[which(Mcvaux[[Nmod]]$lambda==Mcvaux[[Nmod]]$lambda.min)] #error por cv 
    #aux[Nmod,Nmod+3]=Mcvaux[[Nmod]]$cvsd[which(Mcvaux[[Nmod]]$lambda==Mcvaux[[Nmod]]$lambda.min)] #error estándar por cv
  }
  err[Nmod,6]=mean(aux[,Nmod])
}
proc.time()-t1
#user  system elapsed 
#728.39   97.83  865.44 


