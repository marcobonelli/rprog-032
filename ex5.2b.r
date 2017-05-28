#Remove todas as vari�veis do workspace
rm(list=ls(all=TRUE))







#Carrega a biblioteca que permite "exponenciar" uma matriz quadrada
library(expm)

#Carrega as fun��es �teis
source("f.uteis.r")

#Exerc�cio Propriamente Dito
exercicio<-c("Exerc�cio 5.2b")

#Determinando a matriz de transi��o
P<-matrix(0,4,4)
P[1,]<-c(0.90, 0.05, 0.03, 0.02)
P[2,]<-c(0.00, 0.85, 0.09, 0.06)
P[3,]<-c(0.00, 0.00, 0.90, 0.10)
P[4,]<-c(1.00, 0.00, 0.00, 0.00)

#Obtendo o resultado por simula��o
nsim<-10000
soma<-0
n<-3
for(isim in 1:nsim){
	X<-f.gerasequencia(P,n,1)
	if(X[n+1]==4){
		soma<-soma+1
	}
}
prob.simulacao<-soma/nsim

#Obtendo o resultado te�rico
PN<-P%^%n
prob.teorica<-PN[1,4]

#Imprimindo os resultados na tela
f.resultado(exercicio,prob.teorica,prob.simulacao,nsim)