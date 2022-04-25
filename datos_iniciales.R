#PRACTICA FINAL INTELIGENCIA EMPRESARIAL 1º MIT 
#Teresa González García

##########MANIPULACION DATOS ##############
bank <-read.csv("bank-additional.csv",header = TRUE, sep=';')
summary(bank)
str(bank)

#se quita duracion porque hay un sesgo
bank<- bank[,-11] 

#se quitan todos los atributos unkown
bank<-bank[ bank$default!="unknown" & bank$job!="unknown" & bank$marital!="unknown" & bank$loan!="unknown" & bank$housing!="unknown" & bank$education!="unknown", ]
summary(bank)
str(bank)

#CLASES IMBALANCEADAS
imbalance_no <- nrow(bank[bank$y == "no",])/nrow(bank)
imbalance_yes <- nrow(bank[bank$y == "yes",])/nrow(bank)
imbalance_no
imbalance_yes

#ALGORITMO SMOTE

library(rsample)     # for data splitting
library(tidyverse)  # for data processing


# Obtaining Training and Test sets
set.seed(123)
bank$y <- as.factor(bank$y)
bank$age <- as.factor(bank$age)
bank$job <- as.factor(bank$job)
bank$marital <- as.factor(bank$marital)
bank$education <- as.factor(bank$education)
bank$default <- as.factor(bank$default)
bank$housing <- as.factor(bank$housing)
bank$loan <- as.factor(bank$loan)
bank$contact <- as.factor(bank$contact)
bank$month <- as.factor(bank$month)
bank$day_of_week<- as.factor(bank$day_of_week)
bank$campaign <- as.factor(bank$campaign)
bank$pdays <- as.factor(bank$pdays)
bank$previous <- as.factor(bank$previous)
bank$poutcome <- as.factor(bank$poutcome)
bank$emp.var.rate <- as.factor(bank$emp.var.rate)
bank$cons.price.idx<- as.factor(bank$cons.price.idx)
bank$cons.conf.idx <- as.factor(bank$cons.conf.idx)
bank$euribor3m<- as.factor(bank$euribor3m)
bank$nr.employed <- as.factor(bank$nr.employed)

bank_split<- initial_split(bank, prop=0.75)
bank_train<- training(bank_split)
bank_test<- testing(bank_split)

## Let's check the count of unique value in the target variable
table(bank_train$y)

table(bank_test$y)

table(bank_train$y)/nrow(bank_train)

table(bank_test$y)/nrow(bank_test)


## Loading DMwr to balance the unbalanced class

#install.packages("https://cran.r-project.org/src/contrib/Archive/DMwR//DMwR_0.4.1.tar.gz", repos=NULL, type="source")
library("DMwR")


## Smote : Synthetic Minority Oversampling Technique To Handle Class Imbalancy In Binary Classification
bank <- SMOTE(y ~., bank_train, perc.over = 700, k=5, perc.under = 150)
as.data.frame(table(bank$y))
table(bank$y)/nrow(bank)

# Coma como separador y punto como separador decimal. 
write.csv(bank, "bank_balanced.csv")



