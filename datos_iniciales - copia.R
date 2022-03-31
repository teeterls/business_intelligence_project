#PRACTICA FINAL INTELIGENCIA EMPRESARIAL 1º MIT 
#Teresa González García
bank <-read.csv("bank-additional.csv",header = TRUE, sep=';')
summary(bank)
str(bank)

#se quita duracion porque hay un sesgo
bank<- bank[,-11] 

#se quitan todos los atributos unkown
bank<-bank[ bank$default!="unknown" & bank$job!="unknown" & bank$marital!="unknown" & bank$loan!="unknown" & bank$housing!="unknown" & bank$education!="unknown", ]
summary(bank)
str(bank)
