####REDES NEURONALES
library(kohonen) # for building the SOM map
library(caret)     #for confusion matrix
library(tidyverse)  # for data processing
library(neuralnet)  # for MLP training
library(ggplot2)    #for plotting
library(dummies)  #for creration of dummy variables
library(caret) 
library(RSNNS) 

#carga datos
bank <-read.csv("bank_balanced.csv",header = TRUE, sep=',')
bank <-bank[,-1]
target<- as.factor(bank[,20])

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

max<-max(bank[,c(1:19)])
min<-min(bank[,c(1:19)])
#normalizar
bank.sc<-as.matrix(scale(bank[,-20]))

#funcion desnormalizar
denormalize <- function(x) {
  return (x*(max-min) + min)}


set.seed(123)
index <- sample(nrow(bank.sc), round(0.75*nrow(bank.sc)))
bank_train <- bank.sc[index,] 
bank_test <- bank.sc[-index,]

#target para clasificacion
train_label<- as.factor(target[index])
test_label<- target[-index]

##MAPA KOHONEN
#main characteristics of the map
som_grid<-somgrid(xdim=1, ydim=2, topo="hexagonal")

#training the map
bank.som <- som(bank_train, grid=som_grid, 
                rlen=10000, alpha=c(0.05, 0.01), 
                radius= 1, keep.data=T)


# main characteristics of the map
summary(bank.som)

# weights of the map or patterns obtained
bank.som$codes


#Neuron where is training sample belongs
bank.som$unit.classif

#Showing the training process
plot(bank.som, type="changes")

#node counts
plot(bank.som, type="counts", main=" number of examples per neuron")
identify(bank.som) #cliking you can see the value after ESC

#Neighbour distances
plot(bank.som, type="dist.neighbours", main="SOM neighbour distance")
identify(wine.som) #cliking you can see the value after ESC

#Codes/Weight vectors
plot(bank.som, type="codes", main="patterns discovered")

#plotting quality
plot(bank.som, type="quality", main="Node Quality/Distance")

#plotting mapping of data
plot(bank.som, type="mapping", main="Data mapping to the neurons")

coolBlueHOtRed<-function(n, alpha=1){rainbow(n,end=4/6, 
                                             alpha=alpha)[n:1]}

par(mfrow=c(3,2))
plot(bank.som, type="property", property=getCodes(bank.som, 1)[,1],
     main=colnames(getCodes(bank.som, 1))[1],
     palette.name=coolBlueHOtRed)
plot(bank.som, type="property", property=getCodes(bank.som, 1)[,2],
     main=colnames(getCodes(bank.som, 1))[2],
     palette.name=coolBlueHOtRed)
plot(bank.som, type="property", property=getCodes(bank.som, 1)[,3],
     main=colnames(getCodes(bank.som, 1))[3],
     palette.name=coolBlueHOtRed)
plot(bank.som, type="property", property=getCodes(bank.som, 1)[,4],
     main=colnames(getCodes(bank.som, 1))[4],
     palette.name=coolBlueHOtRed)
plot(bank.som, type="property", property=getCodes(bank.som, 1)[,5],
     main=colnames(getCodes(bank.som, 1))[5],
     palette.name=coolBlueHOtRed)
plot(bank.som, type="property", property=getCodes(bank.som, 1)[,6],
     main=colnames(getCodes(bank.som, 1))[6],
     palette.name=coolBlueHOtRed)
########### More heatmaps
par(mfrow=c(3,2))
plot(bank.som, type="property", property=getCodes(bank.som, 1)[,7],
     main=colnames(getCodes(bank.som, 1))[7],
     palette.name=coolBlueHOtRed)
plot(bank.som, type="property", property=getCodes(bank.som, 1)[,8],
     main=colnames(getCodes(bank.som, 1))[8],
     palette.name=coolBlueHOtRed)
plot(bank.som, type="property", property=getCodes(bank.som, 1)[,9],
     main=colnames(getCodes(bank.som, 1))[9],
     palette.name=coolBlueHOtRed)
plot(bank.som, type="property", property=getCodes(bank.som, 1)[,10],
     main=colnames(getCodes(bank.som, 1))[10],
     palette.name=coolBlueHOtRed)
plot(bank.som, type="property", property=getCodes(bank.som, 1)[,11],
     main=colnames(getCodes(bank.som, 1))[11],
     palette.name=coolBlueHOtRed)
plot(bank.som, type="property", property=getCodes(bank.som, 1)[,12],
     main=colnames(getCodes(bank.som, 1))[12],
     palette.name=coolBlueHOtRed)
#Last heatmap
par(mfrow=c(2,2))
plot(bank.som, type="property", property=getCodes(bank.som, 1)[,13],
     main=colnames(getCodes(bank.som, 1))[13],
     palette.name=coolBlueHOtRed)
plot(bank.som, type="property", property=getCodes(bank.som, 1)[,14],
     main=colnames(getCodes(bank.som, 1))[14],
     palette.name=coolBlueHOtRed)
plot(bank.som, type="property", property=getCodes(bank.som, 1)[,15],
     main=colnames(getCodes(bank.som, 1))[15],
     palette.name=coolBlueHOtRed)
plot(bank.som, type="property", property=getCodes(bank.som, 1)[,16],
     main=colnames(getCodes(bank.som, 1))[16],
     palette.name=coolBlueHOtRed)
plot(bank.som, type="property", property=getCodes(bank.som, 1)[,17],
     main=colnames(getCodes(bank.som, 1))[17],
     palette.name=coolBlueHOtRed)
plot(bank.som, type="property", property=getCodes(bank.som, 1)[,18],
     main=colnames(getCodes(bank.som, 1))[18],
     palette.name=coolBlueHOtRed)
plot(bank.som, type="property", property=getCodes(bank.som, 1)[,19],
     main=colnames(getCodes(bank.som, 1))[19],
     palette.name=coolBlueHOtRed)


# Alternative easier
coolBlueHotRed<-function(n, alpha=1){rainbow(n,end=4/6, 
                                             alpha=alpha)[n:1]}

par(mfrow=c(5,3))
for (j in 1:ncol(bank_train)) {
  plot(bank.som, type="property", property=bank.som$codes[[1]][,j],
       palette.name=coolBlueHotRed,
       main=colnames(bank_train)[j], cex=0.5)
}

#Clustering patterns in the map
groups<-2
#Applying hierarchical clustering for grouping patterns
bank.hc=cutree(hclust(dist(bank.som$codes[[1]])), groups)
plot(bank.som, type="codes", bgcol = rainbow(groups)[bank.hc],
     main="clustering the patterns discovered")
add.cluster.boundaries(bank.som,bank.hc)


####### SOM SUPERVISED 
set.seed(7)
kohmap <- xyf(bank_train, train_label,
              grid=som_grid, 
              rlen=100, alpha=c(0.05, 0.01), 
              radius= 1, keep.data=T)

#Showing the training process
plot(kohmap, type="changes")
kohmap$codes
#Showing distribution of labels in neurons
par(mfrow=c(1,2))
plot(kohmap, type="codes",  codeRendering = "lines", shape="straight",
     main=c("housing", "loan"))

#Codes/Weight vectors
plot(kohmap, type="codes", main="patterns discovered")

# Plotting cases in neurons
plot(kohmap, type="mapping", labels=as.numeric(train_label),
     col=as.numeric(train_label)+1, pch=1, main="Map of classes")

plot(kohmap, type="codes", labels=as.numeric(train_label),
     col=as.numeric(train_label)+1, pch=1, main="Map of classes")


##PREDICCIONES
#Prediction using the training data
kohmap.predict_tr<- predict(kohmap, newdata=bank_train, whatmap = 1)

prediction_table_tr<-table(train_label,kohmap.predict_tr$predictions[[2]])

confusionMatrix(prediction_table_tr)

#Prediction using the test data
kohmap.predict<- predict(kohmap, newdata=bank_test, whatmap = 1)

prediction_table<-table(test_label,kohmap.predict$predictions[[2]])

confusionMatrix(prediction_table)


#####REDES NEURONALES NEURALNET
#scatterplots
pairs(bank[15:19])

#plotting some specific relationships for cluster discovering
qplot(age, euribor3m, data=bank,  color=factor(bank$y),
    geom=c("point","smooth"), 
      main = "Euribor3m and Age")


qplot(age, cons.conf.idx, data=bank,  color=factor(bank$y),
      geom=c("point","smooth"), 
      main = "Cons.conf.idx and Age")

qplot(euribor3m, cons.conf.idx, data=bank,  color=factor(bank$y),
      geom=c("point","smooth"), 
      main = "Cons.conf.idx and Euribor3m")


# MODELO NEURALNET bank normalizado train y test y sin target

#primero a partir del banco inicial hay que crear variables dummies para la clase yes y no
bank_dm <- dummy.data.frame(data=bank, names="y", sep="_")
str(bank_dm)

#Normalization of variables
normalization <- function(x) {return ((x-min(x))/(max(x)-min(x)))}

bank_norm <- as.data.frame(lapply(bank_dm, normalization))
str(bank_norm)

#split training y test
index <- sample(nrow(bank_norm), round(0.75*nrow(bank_norm)))
bank_train <- bank_norm[index,] 
bank_test <- bank_norm[-index,]

index_train <- sample(nrow(bank_train), round(0.15*nrow(bank_train)))
bank_train_simp <- bank_train[index_train,] 
bank_test_simp <- bank_train[sample(nrow(bank_train_simp), round(0.15*nrow(bank_train_simp))),] 

ann_model <- neuralnet(y_no+y_yes~age+job+marital+education+default+campaign+previous+pdays+poutcome+euribor3m+nr.employed+cons.conf.idx+cons.price.idx,
                         data=bank_train_simp, hidden=2, 
                       lifesign = "minimal", 
                       linear.output = FALSE, rep =50)

# Visual plot of the model
plot(ann_model, rep="best")

# Best repetition with minimum error
best_rep<-which.min(ann_model$result.matrix[1,])


# Weights for the repetition with the best error
ann_model$weights[best_rep]

# activation function
ann_model$act.fct


# result.matrix: threshold, step, error, AIC, BIC and weights
#plotting the error per repetition
plot(ann_model$result.matrix[1,])
#threshold per repetition
plot(ann_model$result.matrix[2,])

#prediction with training set
pr.nn_tr <- predict(ann_model, bank_train_simp[,1:19], rep=best_rep,all.units=FALSE)
pr.nn_tr_round<-as.data.frame(round(pr.nn_tr))

pred_train<-max.col(pr.nn_tr_round)
real_train<-max.col(bank_train_simp[,20:21])

print(real_train)
print(pred_train)


# Plot real and predicted values for training
plot(real_train,type = "p",col = "red", xlab = "Sample", 
     ylab = "subscribed or not", 
     main = "Training: Real (red) - Predicted (blue)")
lines(pred_train, type = "p", col = "blue")


confusionMatrix(table(real_train,pred_train))



#prediction with TEST set
pr.nn_ts <- predict(ann_model, bank_test_simp[,1:19], rep=best_rep,all.units=FALSE)
pr.nn_ts_round<-as.data.frame(round(pr.nn_ts))

pred_ts<-max.col(pr.nn_ts_round)
real_ts<-max.col(bank_test_simp[,20:21])

# Plot real and predicted values for training
plot(real_ts,type = "p",col = "red", xlab = "Sample", 
     ylab = "wine type", 
     main = "Training: Real (red) - Predicted (blue)")
lines(pred_ts, type = "p", col = "blue")


print(real_ts)
print(pred_ts)
confusionMatrix(table(real_ts,pred_ts))

###RBF MODELS
#Defining inputs and output of the model
bankValues <- bank[,1:19]
bankTargets <- bank[,20]

#DECODING labes of class
bankDecTargets <- decodeClassLabels(bankTargets) #lo vuelve a 1 o a 2

# creation of training and test datasets
banksplit <- splitForTrainingAndTest(bankValues, bankDecTargets, ratio=0.25)

index <- sample(nrow(bank), round(0.85*nrow(bank)))
banksplit$inputsTrain <- bankValues[index,] 
banksplit$targetsTrain <-bankDecTargets[index,]
banksplit$inputsTest <- bankValues[-index,]
banksplit$targetsTest <- bankDecTargets[-index,]

#Normalization of variables
bank_norm <- normTrainingAndTestSet(banksplit)

#training the neural network
rbf_model <- rbf(bank_norm$inputsTrain, bank_norm$targetsTrain, 
                 size = c(40),
                 initFunc="RBF_Weights",
                 initFuncParams=c(0, 1, 0, 0.02, 0.04),
                 learnFunc="RadialBasisLearning",
                 learnFuncParams=c(1e-05, 0, 1e-05, 0.1, 0.8),
                 maxit = 200,
                 updateFunc="Topological_Order",
                 linOut=TRUE)


plotIterativeError(rbf_model)
summary(rbf_model)

#prediction with training set
pr.nn_tr <- predict(rbf_model, bank_norm$inputsTrain)
pr.nn_tr_round<-as.data.frame(round(pr.nn_tr))

pred_train<-max.col(pr.nn_tr_round)
real_train<-max.col(bank_norm$targetsTrain)

library(caret)
print(real_train)
print(pred_train)
confusionMatrix(table(real_train,pred_train))


# Plot real and predicted values for training
plot(real_train,type = "p",col = "red", xlab = "Sample", 
     ylab = "subscribed or not", 
     main = "Training: Real (red) - Predicted (blue)")
lines(pred_train, type = "p", col = "blue")


#prediction with TEST set
pr.nn_ts <- predict(rbf_model, bank_norm$inputsTest)
pr.nn_ts_round<-as.data.frame(round(pr.nn_ts))

pred_ts<-max.col(pr.nn_ts_round)
real_ts<-max.col(bank_norm$targetsTest)

# Plot real and predicted values for training
plot(real_ts,type = "p",col = "red", xlab = "Sample", 
     ylab = "wine type", 
     main = "Training: Real (red) - Predicted (blue)")
lines(pred_ts, type = "p", col = "blue")


print(real_ts)
print(pred_ts)
confusionMatrix(table(real_ts,pred_ts))

weightMatrix(rbf_model)


