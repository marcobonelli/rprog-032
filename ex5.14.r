#Remove todas as vari�veis do workspace
rm(list=ls(all=TRUE))

#Carrega a biblioteca que permite "exponenciar" uma matriz quadrada
library(expm)

#Carrega as fun��es �teis
source("f.uteis.r")

#Exerc�cio Propriamente Dito
exercicio<-c("Exerc�cio 5.14")

#Determinando a matriz de transi��o
P<-matrix(0,6,6)
P[1,]<-c(0.3, 0.5, 0.0, 0, 0, 0.2)
P[2,]<-c(0.0, 0.5, 0.0, 0.5, 0.0, 0.0)
P[3,]<-c(0, 0, 1, 0, 0, 0)
P[4,]<-c(0, 0.3, 0, 0, 0, 0.7)
P[5,]<-c(0.1, 0, 0.1, 0.0, 0.8, 0.0)
P[6,]<-c(0.0, 1.0, 0, 0, 0, 0)

irredutiveis<-list()
irredutiveis[[1]]<-c(2,4,6)
irredutiveis[[2]]<-3
output<-f.matrizRF(P,irredutiveis,TRUE)

print(output)