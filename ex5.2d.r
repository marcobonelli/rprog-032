#Remove todas as variáveis do workspace
rm(list=ls(all=TRUE))

#Carrega a biblioteca que permite "exponenciar" uma matriz quadrada
library(expm)

#Carrega as funções úteis
source("f.uteis.r")

#Exercício Propriamente Dito
exercicio<-c("Exercício 5.2d")




#Determinando a matriz de transição
P<-matrix(0,4,4)
P[1,]<-c(0.90, 0.05, 0.03, 0.02)
P[2,]<-c(0.00, 0.85, 0.09, 0.06)
P[3,]<-c(0.00, 0.00, 0.90, 0.10)
P[4,]<-c(1.00, 0.00, 0.00, 0.00)

#Obtendo o resultado teórico
nestados<-4
nsemanas<-52
prob.est<-(f.probestacionaria(P))
res.teorico<-sum(prob.est[1,1:3])*nsemanas

#Obtendo o resultado por simulação
nsim<-10000
soma<-0
n<-104
for(isim in 1:nsim){
	X<-f.gerasequencia(P,n,1)
	p.falha<-sum(X!=4)/(n+1)
	dias.funcionando<-nsemanas*p.falha
	soma<-soma+dias.funcionando
}
res.simulacao<-soma/nsim

#Imprimindo os resultados na tela
f.resultado(exercicio,res.teorico,res.simulacao,nsim)