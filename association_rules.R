#########REGLAS ASOCIACION#############
install.packages("arules")
library(arules)
#install and load arulesViz
install.packages("arulesViz")
library(arulesViz)
#install and load tidyverse
install.packages("tidyverse")
library(tidyverse)

#install and load knitr
install.packages("knitr")
library(knitr)
#load ggplot2 as it comes in tidyverse
library(ggplot2)
#install and load lubridate
install.packages("lubridate")
library(lubridate)
#install and load plyr
install.packages("plyr")
library(plyr)
library(dplyr)

bank <-read.csv("bank_balanced.csv",header = TRUE, sep=',')
bank <-bank[,-1]
#Convertir a factor
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
summary(bank)
str(bank)
glimpse(bank)
bank_t<-as(bank,"transactions")
bank_t
summary(bank_t)
head(bank_t)

install.packages("RColorBrewer")
#include library RColorBrewer
library(RColorBrewer)

#mayor soporte menos variables candidatas
#VARIOS EJEMPLOS
frequentItems <- eclat (bank_t, parameter = list(supp = 0, 
                                                 maxlen = 1)) # calculates support for frequent items
inspect(frequentItems)

itemFrequencyPlot(bank_t,topN=10,type="absolute",
                  col=brewer.pal(8,'Pastel2'), 
                  main="Absolute Item Frequency Plot")

itemFrequencyPlot(bank_t,topN=10,type="relative",
                  col=brewer.pal(8,'Pastel2'),
                  main="Relative Item Frequency Plot")

itemFrequencyPlot(bank_t, support=.50,  col=brewer.pal(8,'Pastel2'), main="Frequent items support 0.5")
itemFrequencyPlot(bank_t, support=.70,  col=brewer.pal(8,'Pastel2'), main="Frequent items support 0.7")

# REGLAS  min sop 0.4 y min conf 0.6
association.rules <- apriori(bank_t, 
                             parameter = list(supp=0.4, conf=0.6,minlen=2, maxlen=10))

summary(association.rules)
inspect(association.rules[1:10])

plot(association.rules, jitter=0)
plot(association.rules,method="two-key plot")

#Se quitan las redundantes
subset.rules <- which(colSums(is.subset(association.rules, 
                                        association.rules)) > 1) # get subset rules in vector
length(subset.rules)  

#remove
association.rules.new <- 
  association.rules[-subset.rules] # remove subset rules.

length(association.rules.new)
inspect(association.rules.new)
plot(association.rules.new, jitter=0)

# NO ASSOCIATION RULES
no.association.rules <- apriori(bank_t, parameter = 
                                  list(supp=0.4, conf=0.6),
                                appearance = list(default="lhs",rhs="y=no"))

inspect(head(no.association.rules,10))

# YES ASSOCIATION RULES
yes.association.rules <- apriori(bank_t, parameter = 
                                   list(supp=0.12, conf=0.6),
                                 appearance = list(default="lhs",rhs="y=yes"))

inspect(head(yes.association.rules))
