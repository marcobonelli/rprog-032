#Remove todas as variáveis do workspace
rm(list=ls(all=TRUE))

#Carrega a biblioteca que permite "exponenciar" uma matriz quadrada
library(expm)

#Carrega as funções úteis
source("f.uteis.r")

#Exercício Propriamente Dito
exercicio<-c("Exercício 5.5a")

#Determinando a matriz de transição
P<-matrix(0,4,4)
P[1,]<-c(0.20, 0.70, 0.10, 0.00)
P[2,]<-c(0.05, 0.10, 0.05, 0.80)
P[3,]<-c(0.00, 0.00, 1.00, 0.00)
P[4,]<-c(0.00, 0.00, 0.00, 1.00)

#Custos
custo<-matrix(0,4,1)
custo[1,1]<-200
custo[2,1]<-300
custo[3,1]<-50
custo[4,1]<-0

#Obtendo o resultado por simulação
nsim<-1000
soma1<-0
soma2<-0
for(isim in 1:nsim){
	f.progresso(isim,nsim)
	custototal<-0
	pecasboas<-0
	for(ipeca in 1:1000){
		X0<-2
		custopeca<-custo[X0,1]
		while(X0<3){
			X<-f.gerasequencia(P,1,X0)
			X0<-X[2]
			custopeca<-custopeca+custo[X0,1]
		}
		custototal<-custototal+custopeca
		if(X0==4){
			pecasboas<-pecasboas+1
		}
	}
	soma1<-soma1+custototal
	soma2<-soma2+pecasboas
}
custo.pecas<-soma1/nsim
nmedio.pecas.boas<-soma2/nsim
res.simulacao<-(300000+custo.pecas)/nmedio.pecas.boas

#Obtendo o resultado teórico
irredutiveis<-list()
irredutiveis[[1]]<-3
irredutiveis[[2]]<-4
output<-f.matrizRF(P,irredutiveis)

custototal<-300000+1000*(200*output$R[2,1]+300*output$R[2,2]+50*output$F[2,3])
nmedio<-1000*output$F[2,4]
res.teorico<-custototal/nmedio


#Imprimindo os resultados na tela
f.resultado(exercicio,res.teorico,res.simulacao,nsim)