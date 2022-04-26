####ALGORITMOS DE AGRUPACION: CLUSTERING JERARQUICO, KNN, KMEANS####

#librerias
library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization
library(dendextend) # for comparing two dendrograms
library(dummies)  #for creration of dummy variables
library(caret)     #for confusion matrix
library(RSNNS)     #for normalization
library(class)     #class package KNN


#carga datos
bank <-read.csv("bank_balanced.csv",header = TRUE, sep=',')
bank <-bank[,-1]
str(bank)

bank$y <- as.factor(bank$y)
bank$age <- as.numeric(bank$age)
bank$job <- as.numeric(as.factor(bank$job))
bank$marital <- as.numeric(as.factor(bank$marital))
bank$education <- as.numeric(as.factor(bank$education))
bank$default <- as.numeric(as.factor(bank$default))
bank$housing <- as.numeric(as.factor(bank$housing))
bank$loan <- as.numeric(as.factor(bank$loan))
bank$contact <- as.numeric(as.factor(bank$contact))
bank$month <- as.numeric(as.factor(bank$month))
bank$day_of_week<- as.numeric(as.factor(bank$day_of_week))
bank$campaign <- as.numeric(as.factor(bank$campaign))
bank$pdays <- as.numeric(as.factor(bank$pdays))
bank$previous <- as.numeric(as.factor(bank$previous))
bank$poutcome <- as.numeric(as.factor(bank$poutcome))
bank$emp.var.rate <- as.numeric(as.factor(bank$emp.var.rate))
bank$cons.price.idx<- as.numeric(as.factor(bank$cons.price.idx))
bank$cons.conf.idx <- as.numeric(as.factor(bank$cons.conf.idx))
bank$euribor3m<- as.numeric(as.factor(bank$euribor3m))
bank$nr.employed <- as.numeric(as.factor(bank$nr.employed))

str(bank)
bank_d<-scale(bank[-20])
bank_d<-t(bank_d)

# calculo distancia
distance <- get_dist(bank_d)
# visualizacion
fviz_dist(distance, gradient = list(low = "#00AFBB",
                                    mid = "white", high = "#FC4E07"))

#funcion normalizar
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x))) }

max<-max(bank[,c(1:19)])
min<-min(bank[,c(1:19)])

#normalizacion SIN METER EL TARGET
bank_subset_norm<- as.data.frame(lapply(bank[,c(1:19)], normalize))
head(bank_subset_norm)

#GENERACION TRAINING Y TEST CON UN 75% DE SPLIT
set.seed(123)
index <- sample(nrow(bank_subset_norm), round(0.75*nrow(bank_subset_norm)))
bank_train <- bank_subset_norm[index,] 
bank_test <- bank_subset_norm[-index,]

#target para clasificacion
train_label<- bank[index,20]
test_label<- bank[-index,20]

######CLUSTERING JERARQUICO########
distance <- dist(bank_train,method = "euclidean")

# Visualization of a distance matrix
fviz_dist(distance, gradient = list(low = "#00AFBB", 
                                    mid = "white", high = "#FC4E07"))


# Hierarchical clustering using COMPLETE LINKAGE
hc_CL<-hclust(distance, method="complete")

# Plot the obtained dendrogram
plot(hc_CL, cex=0.6, hang=-1)

# AGNES
hc_CL_agnes<-agnes(bank, method = "complete")

#Agglomerative coefficient
hc_CL_agnes$ac

# Plot the obtained dendrogram
pltree(hc_CL_agnes, cex=0.6, hang=-1)

# Comparing different HC methods
# methods to assess
m <- c( "average", "single", "complete", "ward")
names(m) <- c( "average", "single", "complete", "ward")

# function to compute coefficient
ac <- function(x) {
  agnes(bank_train, method = x)$ac
}

# comparacion coef aglom
map_dbl(m, ac)

# AGNES - WARD
#Using the ward method as identified in this case as better
hc_CL_Ward <- agnes(bank_train, method = "ward")
pltree(hc_CL_Ward, cex = 0.6, hang = -1, main = "Dendrogram of agnes - Ward")


#### Sub-Groups identification
# Ward's method
hc_sg <- hclust(distance, method = "ward.D2" )

# Cut tree into 2 groups
sub_grp <- cutree(hc_sg, k = 2)

# Number of members in each cluster
table(sub_grp)

# CLUSTERING Correspondence example - cluster train
assoc_ward<- as.data.frame(bank_train) %>%  mutate(cluster = sub_grp) 
head(assoc_ward)

######## Analysis of clusters
confusionMatrix(as.factor(train_label), as.factor(assoc_ward$cluster))
error_ward<-mean((as.numeric(train_label) - as.numeric(assoc_ward$cluster))^2) #30.75%

#Plotting sub-groups with colors
plot(hc_sg, cex = 0.6)
rect.hclust(hc_sg, k = 2, border = 2:5)


# REPRESENTACION 2 VARIABLES
fviz_cluster(list(data = bank_train, cluster = sub_grp), 
             choose.vars = c("campaign", "age") )


###KNN

sqrt(NROW(bank_train))  # to find the number of observation

knn.62 <-  knn(train=bank_train, test=bank_test, cl=train_label, k=62)
knn.63 <-  knn(train=bank_train, test=bank_test, cl=train_label, k=63)


ACC.62 <- 100 * sum(test_label == knn.62)/NROW(test_label)  # For knn = 26
ACC.63 <- 100 * sum(test_label == knn.63)/NROW(test_label)  # For knn = 27

ACC.62    #Accuracy is 67.67%
ACC.63    #Accuracy is 67.33%, which has reduced compare to k=26



i=1                          # declaration to initiate for loop
k.optm=1                     # declaration to initiate for loop
for (i in 1:70){
  set.seed(123)
  knn.mod <-  knn(train=bank_train, test=bank_test, cl=train_label, k=i)
  k.optm[i] <- 100 * sum(test_label == knn.mod)/NROW(test_label)
  #MSE
  error[i]<-mean((as.numeric(test_label) - as.numeric(knn.mod))^2)
  k=i
  cat(k,'=',k.optm[i],'\n')       # to print % accuracy
}
plot(k.optm, type="b", xlab="K- Value",ylab="Accuracy level")  # to plot % accuracy wrt to k-value
plot(error, type="b", xlab="K- Value",ylab="MSE")  # to plot % accuracy wrt to k-value

#SE ESCOGE EL VALOR  K=6 porque tiene un accuracy level de 81.18%
knn.6 <-  knn(train=bank_train, test=bank_test, cl=train_label, k=6)

table(knn.6 ,test_label) 

confusionMatrix(knn.6 ,as.factor(test_label))

#CV LEAVE ONE OUT

knn.cross<-knn.cv(train=bank_train, cl=train_label, k=6 ,prob=TRUE)
confusionMatrix(knn.cross,train_label)
error_cv<-mean((as.numeric(train_label) - as.numeric(knn.cross))^2) #17.80%

#####KMEANS######### 

#Metodo Elbow k optimo
fviz_nbclust(bank_subset_norm, kmeans, method = "wss", k.max = 20)

#Metodo Silhouette k optimo
fviz_nbclust(bank_subset_norm, kmeans, method = "silhouette")

# SE ESCOGE K=2 AUNQUE NO ES EL OPTIMO, PARA QUE COINCIDA CON las 2 variables target (binaria)
cluster2<-kmeans(bank_subset_norm,centers=2,nstart=20)

# clustering results
str(cluster2)
cluster2

cluster2$size

#visualization  of clusters 
fviz_cluster(cluster2, data=bank_subset_norm,
             choose.vars = colnames(bank_subset_norm[, c("age","euribor3m", "job")]))


fviz_cluster(cluster2, data=bank_subset_norm,
             choose.vars = colnames(bank_subset_norm[, c("age","euribor3m", "job")]),stand = FALSE, 
             ellipse.type = "norm") + theme_bw()


#denormalizing centers to obtain real values
codes<- cluster2$centers

#desnormalizar
#funcion normalizar
denormalize <- function(x) {
  return (x*(max-min) + min)}


#normalizacion SIN METER EL TARGET
denorm_codes_1<- lapply(codes[1,], denormalize)
denorm_codes_1<-as.factor(denorm_codes_1)
denorm_codes_2<- lapply(codes[2,], denormalize)


# PREDICCION

assigned_cluster <- cluster2$cluster

bank_subset_norm$assigned_cluster<- cluster2$cluster

data_clus_1 <- bank_train[assigned_cluster == 1,] #distance to the assigned cluster
data_clus_1L <- train_label[assigned_cluster == 1] #no
data_clus_1L

data_clus_2 <- bank_train[assigned_cluster == 2,]
data_clus_2L <- train_label[assigned_cluster == 2] #yes
data_clus_2L




############ Prediction of clusters and target
clusters <- function(x, centers) {
  # compute squared euclidean distance from each sample to each cluster center
  tmp <- sapply(seq_len(nrow(x)),
                function(i) apply(centers, 1,
                                  function(v) sum((x[i, ]-v)^2)))
  max.col(-t(tmp))  # find index of min distance
}



################################  TRAINING case
# new data to predict
prediction<- clusters(bank_train, cluster2[["centers"]])



table_pred<-table(train_label, prediction)
confusionMatrix(as.factor(train_label), as.factor(prediction))
error_kmeans<-mean((as.numeric(train_label) - as.numeric(prediction))^2) #48.65%

################################  TEST case
# new data to predict
prediction<- clusters(bank_test, cluster2[["centers"]])

#colnames(prediction) <- colnames(test)

table_pred<-table(test_label, prediction)
confusionMatrix(as.factor(test_label), as.factor(prediction))
error_kmeans<-mean((as.numeric(test_label) - as.numeric(prediction))^2) #47.49%

