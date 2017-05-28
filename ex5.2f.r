#Remove todas as vari�veis do workspace
rm(list=ls(all=TRUE))

#Carrega a biblioteca que permite "exponenciar" uma matriz quadrada
library(expm)

#Carrega as fun��es �teis
source("f.uteis.r")

#Exerc�cio Propriamente Dito
exercicio<-c("Exerc�cio 5.2f")

#Determinando a matriz de transi��o
P<-matrix(0,4,4)
P[1,]<-c(0.90, 0.05, 0.03, 0.02)
P[2,]<-c(0.00, 0.85, 0.09, 0.06)
P[3,]<-c(1.00, 0.00, 0.00, 0.00)
P[4,]<-c(1.00, 0.00, 0.00, 0.00)

#Obtendo o resultado te�rico
custo<-matrix(0,4,1)
custo[1,1]<-1000
custo[2,1]<-500
custo[3,1]<- -600
custo[4,1]<- -700

nestados<-4
nsemanas<-52
prob.est<-(f.probestacionaria(P))
res.teorico<-prob.est%*%custo

#Obtendo o resultado por simula��o
nsim<-10000
soma<-0
n<-52
for(isim in 1:nsim){
	X0<-sample.int(nestados,1,replace=FALSE,prob=prob.est)
	X<-f.gerasequencia(P,n,X0)
	custo.observado<-0
	for(j in 1:nestados){
		custo.estado<-sum(X==j)*custo[j,1]
		custo.observado<-custo.observado+custo.estado
	}
	soma<-soma+custo.observado/(n+1)
}
res.simulacao<-soma/nsim

#Imprimindo os resultados na tela
f.resultado(exercicio,res.teorico,res.simulacao,nsim)