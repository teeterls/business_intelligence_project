###ARBOLES DE DECISION: C4.5, CART, RF, BOOSTING

#librerias
library(C50) #C4.5 algoritmo
library(tidyverse)
library(rpart)      # for CART decision tree
library(rpart.plot) # for plotting CART
library(caret)      # for confusion matrix and more
library(rsample)    # for data splitting
library(randomForest)  # For bagging and randomforest
library(ggpubr)

#carga datos
bank <-read.csv("bank_balanced.csv",header = TRUE, sep=',')
bank <-bank[,-1]

bank$y <- as.factor(bank$y)

#GENERACION TRAINING Y TEST CON UN 75% DE SPLIT
set.seed(123)
index <- sample(nrow(bank), round(0.75*nrow(bank)))
bank_train <- bank[index,] 
bank_test <- bank[-index,]

table(bank_train$y)

table(bank_test$y)

table(bank_train$y)/nrow(bank_train)

table(bank_test$y)/nrow(bank_test)


####ALGORITMO C4.5#########
# Creating the decision tree algorithm C4.5 CON TRAIN
tree_result_C45 <- C5.0(y  ~ ., data=bank_train,
                    control = C5.0Control(
                      noGlobalPruning = FALSE, # Pruning is in effect
                      CF= 0.25))  #Higher CF less prunning


summary(tree_result_C45)

#PLotting the tree
plot(tree_result_C45, trial=0, subtree=NULL)

#TEST NUEVOS CASOS
predictions_C45 <- predict(tree_result_C45, newdata = bank_test, type ="class")

table(prediction=predictions_C45, real= bank_test$y)

error_classification_C45 <- mean(predictions_C45 != bank_test$y) #10.62%
error_classification_C45_MSE <-mean((as.numeric(predictions_C45) - as.numeric(bank_test$y))^2)#10.62%

paste("The classification error is:", 100*error_classification_C45, "%",
      sum(predictions_C45==bank_test$y),
      "correct classified cases from", length(predictions_C45))

#Obtaining Knowledge rules

# Use this expression below
ruleModel <- C5.0(y ~ ., data = bank_train, rules = TRUE)


ruleModel

summary(ruleModel)


#####ALGORITMO CART######
# Creating the decision tree algorithm in CART
tree_result_CART<- rpart(formula=y  ~ ., data=bank_train, method='class')

#Resulting tree
print(tree_result_CART)

# Alternative views
summary(tree_result_CART)

#Representaciones
prp(tree_result_CART, faclen=3, clip.facs=TRUE, 
    split.fun=split.fun, tweak=1.2, extra=2)

prp(tree_result_CART, faclen=3, clip.facs=TRUE, 
    split.fun=split.fun, tweak=1.2, extra=101)

#OBTENER REGLAS CONOCIMIENTO
rpart.rules(tree_result_CART, style = "tall", cover=TRUE,
            nn=TRUE, clip.facs = TRUE)


#Prediction of the training cases from the train dataset
pred_train <- predict(tree_result_CART, newdata = bank_train, type ="class")
confusionMatrix(pred_train, bank_train$y)


error_classification_CART <- mean(pred_train != bank_train$y)


#Prediction of new cases from the test dataset
predictions <- predict(tree_result_CART, newdata = bank_test, type ="class")
confusionMatrix(predictions, bank_test$y)

error_classification_CART <- mean(predictions != bank_test$y)


#PrUNNING
tree_pruned<- prune(tree_result_CART, cp=0.02)
rpart.plot(tree_pruned, type=1, branch=0,tweak=2.3, 
           fallen.leaves = TRUE,
           varlen = 0, faclen = 0, split.fun=split.fun)

prp(tree_pruned, faclen=3, clip.facs=TRUE, 
    split.fun=split.fun, tweak=1.2, extra=101)


#Prediction of the training cases from the train dataset PRUNING
pred_train <- predict(tree_pruned, newdata = bank_train, type ="class")
confusionMatrix(pred_train, bank_train$y)

error_classification <- mean(pred_train != bank_train$y)


#Prediction of new cases from the test dataset
predictions <- predict(tree_pruned, newdata = bank_test, type ="class")
confusionMatrix(predictions, bank_test$y)

error_classification <- mean(predictions != bank_test$y)


# Analysis of cp values in a table
printcp(tree_result_CART, digits=4)


# Error evolution with increasing number of nodes
plotcp(tree_result_CART, lty=2 , col="red", upper="size" )
plotcp(tree_result_CART, lty=2 , col="red", upper="splits" )


#Prunning analysis with cp 0.011
tree_pruned<- prune(tree_result_CART, cp=0.011)
rpart.plot(tree_pruned, type=1, branch=0,tweak=1.8, 
           fallen.leaves = TRUE,
           varlen = 0, faclen = 0, split.fun=split.fun)

prp(tree_pruned, faclen=3, clip.facs=TRUE, 
    split.fun=split.fun, tweak=1.2, extra=101)

#Prediction of the training cases from the train dataset PRUNING
pred_train <- predict(tree_pruned, newdata = bank_train, type ="class")
confusionMatrix(pred_train, bank_train$y)

table(prediction=pred_train, real= bank_train$y)

error_classification <- mean(pred_train != bank_train$y)


#Prediction of new cases from the test dataset
predictions <- predict(tree_pruned, newdata = bank_test , type ="class")
confusionMatrix(predictions, bank_test$y)

error_classification <- mean(predictions != bank_test$y)



##RANDOM FOREST#####
#inicialmente valor de m=raiz numero atributos = 5
# Creating the random forest model
bagging_model<- randomForest(formula=y  ~ ., data=bank_train, 
                             mtry=5)  #5 from 20 predictors will be selected


#Result of random forest model
print(bagging_model)

#Valores de los hiperparametros optimos
#Tunning the m number of predictors
tuning_rf_mtry <- function(df, y, ntree = 500){
  # This function returns the out-of-bag-MSE of a RandomForest model
  # in function of the number of predictors evaluated
  
  
  # Arguments:
  #   df = data frame with predictors and variable to predict
  #   y  = name of the variable to predict
  #   ntree = number of trees created by the randomForest algorithm
  
  require(dplyr)
  max_predictors <- ncol(df) - 1
  n_predictors   <- rep(NA, max_predictors)
  oob_err_rate   <- rep(NA, max_predictors)
  for (i in 1:max_predictors) {
    set.seed(123)
    f <- formula(paste(y,"~ ."))
    model_rf <- randomForest(formula = f, data = df, mtry = i, ntree = ntree)
    n_predictors[i] <- i
    oob_err_rate[i] <- tail(model_rf$err.rate[,1], n = 1)
  }
  results <- data_frame(n_predictors, oob_err_rate)
  return(results)
}

hiperparameter_mtry <-  tuning_rf_mtry(df = bank_train, y = "y")
hiperparameter_mtry %>% arrange(oob_err_rate)

ggplot(data = hiperparameter_mtry, aes(x = n_predictors, y = oob_err_rate)) +
  scale_x_continuous(breaks = hiperparameter_mtry$n_predictors) +
  geom_line() +
  geom_point() +
  geom_point(data = hiperparameter_mtry %>% arrange(oob_err_rate) %>% head(1),
             color = "red") +
  labs(title = "Evolution of the out-of-bag-error vs m",
       x = "number of predictors used") +
  theme_bw()

#M OPTIMO = 4
bagging_model<- randomForest(formula=y  ~ ., data=bank_train, 
                             mtry=4)  #4 optimo


#Result of random forest model
print(bagging_model)

# TAMAÑO NODOS OPTIMO

tuning_rf_nodesize <- function(df, y, size = NULL, ntree = 500){
  # This funstion returns the out-of-bag-MSE of a random forestmodel
  # in function of the minimum size of the terminal nodes (nodesize).
  
  
  # Arguments:
  #   df = data frame with predictors and variable to predict
  #   y  = name of the variable to predict
  #   size= evaluated sizes
  #   ntree = number of trees created by the randomForest algorithm
  
  
  require(dplyr)
  if (is.null(size)){
    size <- seq(from = 1, to = nrow(df), by = 5)
  }
  
  oob_err_rate <- rep(NA, length(size))
  for (i in seq_along(size)) {
    set.seed(123)
    f <- formula(paste(y,"~ ."))
    model_rf <- randomForest(formula = f, data = df, mtry = 5, ntree = ntree,
                             nodesize = i)
    oob_err_rate[i] <- tail(model_rf$err.rate[, 1],n = 1)
  }
  results <- data_frame(size, oob_err_rate)
  return(results)
}

hiperparameter_nodesize <-  tuning_rf_nodesize(df = bank_train, y = "y",
                                               size = c(1:20))
hiperparameter_nodesize %>% arrange(oob_err_rate)


ggplot(data = hiperparameter_nodesize, aes(x = size, y = oob_err_rate)) +
  scale_x_continuous(breaks = hiperparameter_nodesize$size) +
  geom_line() +
  geom_point() +
  geom_point(data = hiperparameter_nodesize %>% arrange(oob_err_rate) %>% head(1),
             color = "red") +
  labs(title = "Evolución del out-of-bag-error vs nodesize",
       x = "number of observationes in terminal nodes") +
  theme_bw()

#M OPTIMO = 4 NSIZE=1
bagging_model<- randomForest(formula=y  ~ ., data=bank_train, 
                             mtry=4, nodesize=1) 


#Result of random forest model
print(bagging_model)

model_randomforest <- randomForest(y ~ ., data = bank_train,
                                   mtry = 4 , ntree = 500, nodesize = 1,
                                   importance = TRUE)
# NTREES OPTIMO
oob_error_rate <- data.frame(oob_error_rate = model_randomforest$err.rate[,1],
                             trees = seq_along(model_randomforest$err.rate[,1]))


ggplot(data = oob_error_rate, aes(x = trees, y = oob_error_rate )) +
  geom_line() +
  labs(title = "Evolution of the out-of-bag-error vs trees number",
       x = "number of trees") +
  theme_bw()

#MODELO OPTIMIZADO
# Creating the random forest model
rf_model<- randomForest(formula=y  ~ ., data=bank_train, 
                        mtry=4, ntree=350, nodesize=1,
                        importance=TRUE, norm.votes=TRUE)



#Result of random forest model
print(rf_model)

# PREDICTORES SIGNIFICATIVOS TARGET en funcion de pureza y precision
importance_pred <- as.data.frame(importance(rf_model, scale = TRUE))
importance_pred <- rownames_to_column(importance_pred, 
                                      var = "variable")

print(importance_pred)


p1 <- ggplot(data = importance_pred, 
             aes(x = reorder(variable, MeanDecreaseAccuracy), 
                 y = MeanDecreaseAccuracy, fill = MeanDecreaseAccuracy)) +
  labs(x = "variable", title = "Accuracy Reduction") +
  geom_col() +
  coord_flip() +
  theme_bw() +
  theme(legend.position = "bottom")

p2 <- ggplot(data = importance_pred, 
             aes(x = reorder(variable, MeanDecreaseGini), 
                 y = MeanDecreaseGini, fill = MeanDecreaseGini)) +
  labs(x = "variable", title = "Purity Reduction (Gini)") +
  geom_col() +
  coord_flip() +
  theme_bw() +
  theme(legend.position = "bottom")

ggarrange(p1,p2)

#EVALUACION PREDICCION

#Prediction of the training cases from the train dataset PRUNING
pred_train <- predict(rf_model, newdata = bank_train, type ="class")
confusionMatrix(pred_train, bank_train$y)

error_classification <- mean(pred_train != bank_train$y)


#Prediction of new cases from the test dataset
predictions <- predict(rf_model, newdata = bank_test, type ="class")
confusionMatrix(predictions, bank_test$y)

table(prediction=predictions, real= churnTest$churn)

error_classification <- mean(predictions != bank_test$y)

