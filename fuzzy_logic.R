####LOGICA BORROSA
library(frbs)       # fuzzy package
library(caret)     #for confusion matrix

#carga datos
bank <-read.csv("bank_balanced.csv",header = TRUE, sep=',')
bank <-bank[,-1]
bank$y <- as.numeric(as.factor(bank$y))
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

# data set preparation. Move column wine 1 to 14 and randomization of cases
bank <- bank[sample(nrow(bank)), ]  # random ordering

#a<-wines[,1]  #dummy variable for replacement of column 13
#wines[,1:13]<-wines[,2:13]
#wines[,14]<- a  # Now coulm 14 is type of wine. Objective to classify


#re-assigning names for attributes from wine.names


# creation of training and test datasets
bank_train <- bank[1 : 3000, ]  # we selec 118 random cases for training
# OBSERVE that taining set include the wine column but not 
# the test dataset
bank_test <- bank[3001 : nrow(bank), 1 : 19]

real_test <- matrix(bank[3001 : nrow(bank), 19], ncol = 1)

## Define range of input data. Note that it is only for the input variables.
range.data.input <-apply(bank, 2, range) # Return max & min values for normalization and labelling
# wine is included

## Set the method and its parameters. In this case we use FRBCS.W algorithm
method.type <- "FRBCS.W"
control <- list(num.labels = 3, type.mf = "GAUSSIAN", type.tnorm = "MIN",  # NOTE number of labels
                type.snorm = "MAX", type.implication.func = "ZADEH")  

## Learning step: Generate fuzzy model for classification
object.cls <- frbs.learn(bank_train, range.data.input, method.type, control)

## Display the FRBS model

object.cls$rule

#summary(object.cls)

## Plot the membership functions
#plotMF(object.cls)

# Performance with training data set


index_train <- sample(nrow(bank_train), round(0.15*nrow(bank_train)))
bank_train_simp <- bank_train[index_train,] 
bank_test_simp <- bank_train[sample(nrow(bank_train_simp), round(0.15*nrow(bank_train_simp))),] 

pred_train<- predict(object.cls, bank_train_simp[,1:19])

pred_train <- pred_train[,1]
real_train <- bank_train_simp[,20]

table_pred<-table(real_train, pred_train)

confusionMatrix(as.factor(real_train), as.factor(pred_train))


# Performance with test data set

pred_test<- predict(object.cls, bank_test_simp[,1:19])

pred_test <- pred_test[,1]
real_test <- bank_test_simp[,20]

table_pred<-table(real_test, pred_test)

confusionMatrix(as.factor(real_test), as.factor(pred_test))
