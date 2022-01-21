####TAREA 2: ANÁLISIS DE COMPONENTES PRINCIPALES Y CONGLOMERADOS 
library(ISLR)
library(dplyr)
library(MASS)
library(ggplot2)
library(ggdendro)
library(dendextend)

#Para hacer dendogramas####
dendro_data_k <- function(hc, k) {
  
  hcdata    <-  ggdendro::dendro_data(hc, type = "rectangle")
  seg       <-  hcdata$segments
  labclust  <-  cutree(hc, k)[hc$order]
  segclust  <-  rep(0L, nrow(seg))
  heights   <-  sort(hc$height, decreasing = TRUE)
  height    <-  mean(c(heights[k], heights[k - 1L]), na.rm = TRUE)
  for (i in 1:k) {
    xi      <-  hcdata$labels$x[labclust == i]
    idx1    <-  seg$x    >= min(xi) & seg$x    <= max(xi)
    idx2    <-  seg$xend >= min(xi) & seg$xend <= max(xi)
    idx3    <-  seg$yend < height
    idx     <-  idx1 & idx2 & idx3
    segclust[idx] <- i
  }
  
  idx                    <-  which(segclust == 0L)
  segclust[idx]          <-  segclust[idx + 1L]
  hcdata$segments$clust  <-  segclust
  hcdata$segments$line   <-  as.integer(segclust < 1L)
  hcdata$labels$clust    <-  labclust
  
  hcdata
}
set_labels_params <- function(nbLabels,
                              direction = c("tb", "bt", "lr", "rl"),
                              fan       = FALSE) {
  if (fan) {
    angle       <-  360 / nbLabels * 1:nbLabels + 90
    idx         <-  angle >= 90 & angle <= 270
    angle[idx]  <-  angle[idx] + 180
    hjust       <-  rep(0, nbLabels)
    hjust[idx]  <-  1
  } else {
    angle       <-  rep(0, nbLabels)
    hjust       <-  0
    if (direction %in% c("tb", "bt")) { angle <- angle + 45 }
    if (direction %in% c("tb", "rl")) { hjust <- 1 }
  }
  list(angle = angle, hjust = hjust, vjust = 0.5)
}
plot_ggdendro <- function(hcdata,
                          direction   = c("lr", "rl", "tb", "bt"),
                          fan         = FALSE,
                          scale.color = NULL,
                          branch.size = 1,
                          label.size  = 3,
                          nudge.label = 0.01,
                          expand.y    = 0.1) {
  
  direction <- match.arg(direction) # if fan = FALSE
  ybreaks   <- pretty(segment(hcdata)$y, n = 5)
  ymax      <- max(segment(hcdata)$y)
  
  ## branches
  p <- ggplot() +
    geom_segment(data         =  segment(hcdata),
                 aes(x        =  x,
                     y        =  y,
                     xend     =  xend,
                     yend     =  yend,
                     linetype =  factor(line),
                     colour   =  factor(clust)),
                 lineend      =  "round",
                 show.legend  =  FALSE,
                 size         =  branch.size)
  
  ## orientation
  if (fan) {
    p <- p +
      coord_polar(direction = -1) +
      scale_x_continuous(breaks = NULL,
                         limits = c(0, nrow(label(hcdata)))) +
      scale_y_reverse(breaks = ybreaks)
  } else {
    p <- p + scale_x_continuous(breaks = NULL)
    if (direction %in% c("rl", "lr")) {
      p <- p + coord_flip()
    }
    if (direction %in% c("bt", "lr")) {
      p <- p + scale_y_reverse(breaks = ybreaks)
    } else {
      p <- p + scale_y_continuous(breaks = ybreaks)
      nudge.label <- -(nudge.label)
    }
  }
  
  # labels
  labelParams <- set_labels_params(nrow(hcdata$labels), direction, fan)
  hcdata$labels$angle <- labelParams$angle
  
  p <- p +
    geom_text(data        =  label(hcdata),
              aes(x       =  x,
                  y       =  y,
                  label   =  label,
                  colour  =  factor(clust),
                  angle   =  angle),
              vjust       =  labelParams$vjust,
              hjust       =  labelParams$hjust,
              nudge_y     =  ymax * nudge.label,
              size        =  label.size,
              show.legend =  FALSE)
  
  # colors and limits
  if (!is.null(scale.color)) {
    p <- p + scale_color_manual(values = scale.color)
  }
  
  ylim <- -round(ymax * expand.y, 1)
  p    <- p + expand_limits(y = ylim)
  
  p
}
set.seed(2610)
######Ejercicio 3######
df <- data.frame(x1 = c(1,1,0,5,6,4), x2= c(4,3,4,1,2,0))
#a) Gráfica de las obs
ggplot(df,aes(x1,x2)) +
  geom_point(col="green",size=5) +
  xlab("X1") + ylab("X2") + ggtitle("Observaciones") + theme_bw()
#b) Asignamos un grupo a cada variable
df$grupo <- sample(c(1,2),size = 6,replace = TRUE);df
#Calculamos el centroide de cada grupo (c1,c2)
c <- data.frame(df %>% group_by(grupo) %>% summarize(c1 = mean(x1),c2 = mean(x2)));c
#Reasignamos obs al grupo con el centroide más cercano
d1 <- c(); d2 <- c()
for (i in 1:6) {
  d1[i] <- sqrt((df[i,1]-c[1,2])^2 + (df[i,2]-c[1,3])^2)
  d2[i] <- sqrt((df[i,1]-c[2,2])^2 + (df[i,2]-c[2,3])^2)
  if(d1[i] < d2[i]){df$grupo2[i] <- 1}else{df$grupo2[i] <- 2}
};df
#Repetimos el procedimiento
c2 <- data.frame(df %>% group_by(grupo2) %>% summarize(c1 = mean(x1),c2 = mean(x2)))
d1 <- c(); d2 <- c()
for (i in 1:6) {
  d1[i] <- sqrt((df[i,1]-c2[1,2])^2 + (df[i,2]-c2[1,3])^2)
  d2[i] <- sqrt((df[i,1]-c2[2,2])^2 + (df[i,2]-c2[2,3])^2)
  if(d1[i] < d2[i]){df$grupo3[i] <- 1}else{df$grupo3[i] <- 2}
};df
#Agrupamiento final
ggplot(df,aes(x1,x2,col= as.factor(grupo3))) +
  geom_point(size=5) +
  xlab("X1") + ylab("X2") + ggtitle("Observaciones") +
  labs(colour = "Grupo") + theme_bw()

######Ejercicio 9#######
data("USArrests")
USA.scaled=scale(USArrests)
##Haciendo agrupación jerárquica por método complete 
clust.complete=hclust(dist(USArrests),method='complete')
data <- dendro_data_k(clust.complete, 3)
p <- plot_ggdendro(data,
                   direction   = "bt",
                   expand.y    = 0.2)
p<-p+ggtitle("Dendograma USArrests")+theme(plot.title = element_text(hjust = 0.5))+labs(x="",y="")
p
###Hacemos un corte en k=3
clust.complete.cut=cutree(clust.complete,k = 3)
#Vemos las agrupaciones
split(data.frame(clust.complete.cut),as.factor(clust.complete.cut))
aggregate(USArrests,list(clust.complete.cut=clust.complete.cut),mean)
#Agrupación jerárquica después de reescalar
clust.scaled=hclust(dist(USA.scaled),method='complete')
data1 <- dendro_data_k(clust.scaled, 3)
p1 <- plot_ggdendro(data1,
                    direction   = "bt",
                    expand.y    = 0.2)
p1<-p1+ggtitle("Dendograma USArrests escalado")+theme(plot.title = element_text(hjust = 0.5))+labs(x="",y="")
p1
#Corte k=3
clust.scaled.cut=cutree(clust.scaled,k = 3)
split(data.frame(clust.scaled.cut),as.factor(clust.scaled.cut))
aggregate(USArrests,list(clust.scaled.cut=clust.scaled.cut),mean)

######Ejercicio 10######
##20 obs de 3 categorías (60 obs) en 50 variables##
df2<-data.frame(grupo=c(rep("A",20),rep("B",20),rep("C",20)))
for (i in 1:50) {
  df2[1:20,i+1] <- rnorm(20,mean=0);df2[21:40,i+1] <- rnorm(20,mean=1);df2[41:60,i+1] <- rnorm(20,mean=2)
}
##Componentes principales##
pc <- prcomp(df2[,-1]); pc_s <- summary(pc)
pc_s$importance[,1:5]
##Gráfica de los primeros dos componentes##
color <- c("#d3a3d3","#8ad3e6","#f37dc0")
plot(predict(pc)[,1:2], col = color[df2$grupo],pch=19,cex=1.5)
##Hacemos k-means con k=3##
km3 <- kmeans(df2[,-1],centers = 3,nstart = 3)
table(km3$cluster) #Cada grupo formado tiene 20 obs.
#Seleccionamos las observaciones que formaron cada grupo
g1<-df2[km3$cluster==1,];g2<-df2[km3$cluster==2,];g3<-df2[km3$cluster==3,]
cbind(distinct(g1,grupo),distinct(g2,grupo),distinct(g3,grupo)) #Cada grupo formado tiene un solo tipo
#de grupo original, es decir, la separación es correcta.
##k-means con k=2##
km2 <- kmeans(df2[,-1],centers = 2,nstart = 2);table(km2$cluster)
g1<-df2[km2$cluster==1,];g2<-df2[km2$cluster==2,]
table(g1$grupo);table(g2$grupo)
##k-means con k=4##
km4 <- kmeans(df2[,-1],centers = 4,nstart = 4);table(km4$cluster)
g1<-df2[km4$cluster==1,];g2<-df2[km4$cluster==2,];g3<-df2[km4$cluster==3,];g4<-df2[km4$cluster==4,]
rbind(table(g1$grupo),table(g2$grupo),table(g3$grupo),table(g4$grupo))
##k-means con k=3 en los primeros dos componentes principales##
km3_pc <- kmeans(pc$x[,1:2],centers = 3,nstart = 3)
#Gráfica de los grupos formados con k-means
par(mfrow=c(1,2))
plot(predict(pc)[,1:2],col = color[km3_pc$cluster],pch=19,main = "k = 3")
points(km3_pc$centers, pch=19,cex = 2) #centroides
plot(predict(pc)[,1:2],col = color[df2$grupo],pch=19,main="Grupos originales") 
##k-means con obs escaladas
df2_sc <- scale(df2[,-1])
km3_sc <- kmeans(df2_sc,centers = 3,nstart = 3);table(km3_sc$cluster)
g1<-df2[km3_sc$cluster==1,];g2<-df2[km3_sc$cluster==2,];g3<-df2[km3_sc$cluster==3,]
rbind(table(g1$grupo),table(g2$grupo),table(g3$grupo))

######Ejercicio 11#####
datos = read.csv("C:\\Users\\Yanely\\Documents\\2021-1\\Aprendizaje Estadístico Automatizado - Seminario de Estadística\\Ch10Ex11.csv", header=F)
par(mfrow = c(2,2))
#Agrupación jerárquica método single
hclust.single=hclust(as.dist(1-cor(datos)),method='single')
data2 <- dendro_data_k(hclust.single, 2)
p2 <- plot_ggdendro(data2,
                    direction   = "lr",
                    expand.y    = 0.2)
p2<-p2+ggtitle("Dendograma single")+theme(plot.title = element_text(hjust = 0.5))+labs(x="",y="")
p2
#Método complete
hclust.complete=hclust(as.dist(1-cor(datos)),method = 'complete')
data3 <- dendro_data_k(hclust.complete, 2)
p3 <- plot_ggdendro(data3,
                    direction   = "lr",
                    expand.y    = 0.2)
p3<-p3+ggtitle("Dendograma complete")+theme(plot.title = element_text(hjust = 0.5))+labs(x="",y="")
p3
#Método centroid
hclust.centroid=hclust(as.dist(1-cor(datos)),method = 'centroid')
data4 <- dendro_data_k(hclust.centroid, 2)
p4 <- plot_ggdendro(data4,
                    direction   = "lr",
                    expand.y    = 0.2)
p4<-p4+ggtitle("Dendograma centroid")+theme(plot.title = element_text(hjust = 0.5))+labs(x="",y="")
p4
#Método average
hclust.average=hclust(as.dist(1-cor(datos)),method = 'average')
data5 <- dendro_data_k(hclust.average, 2)
p5 <- plot_ggdendro(data5,
                    direction   = "lr",
                    expand.y    = 0.2)
p5<-p5+ggtitle("Dendograma average")+theme(plot.title = element_text(hjust = 0.5))+labs(x="",y="")
p5
#Hacemos análisis de componentes principales
genes <- prcomp(t(datos))
summary(genes)
head(genes$rotation)
#Buscamos los genes más diferentes
total <- apply(genes$rotation, 1, sum)
idx <- order(abs(total), decreasing = TRUE)
#Buscamos los primeros 10
idx[1:10]

hclust.complete.cut <- cutree(hclust.complete,2)
s <- split(data.frame(hclust.complete.cut),as.factor(hclust.complete.cut))
data.frame(grupo1 <- s$`1`,grupo2 = s$`2`)
d <- aggregate(t(datos),list(hclust.complete.cut=hclust.complete.cut),mean)
difd <- abs(d[1,] - d[2,])
idx2 <- order(difd,decreasing = TRUE)
idx2[1:10]-1


grupos <- c(rep(1,20),rep(2,20))
split(data.frame(grupos),as.factor(grupos))
d2 <- aggregate(t(datos),list(grupos = grupos),mean)
difd2 <- abs(d2[1,] - d2[2,])
idx3 <- order(difd2,decreasing = TRUE)
idx3[1:10]-1