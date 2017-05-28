f.gerasequencia<-function(P,n,X0){

	#Obtendo o número de estados
	nestados<-ncol(P)

	#Inicializando o vertor da sequência dos estados
	X<-rep(0,n+1)
	X[1]<-X0

	#Gerando a sequência de estados
	for(i in 1:n){
		origem<-X[i]
		probs<-P[origem,]
		destino<-sample.int(nestados,1,replace=FALSE,prob=probs)
		X[i+1]<-destino
	}
	
	return(X)

}

f.resultado<-function(exercicio,res.teorico,res.simulacao,nsim){
	print(noquote("=============================================================="))
	print(noquote(exercicio))
	print(noquote(sprintf("Resultado Teórico : %.6f", res.teorico)))
	print(noquote(sprintf("Resultado Simulado: %.6f, (%d simulações)", res.simulacao,nsim)))
	print(noquote("=============================================================="))
}

f.probestacionaria<-function(P){
	n<-nrow(P)
	I<-diag(n)
	A<-t(P)-I
	B<-matrix(0,n,1)
	A[n,]<-1
	B[n,]<-1
	X<-solve(A,B)
	pe<-t(X)
	return(pe)
}

f.progresso<-function(i,nsim,limpar=TRUE){
	passo<-floor(nsim/100)
	if(passo==1){
		passo<-1
	}
	if((i %% passo)==0 || i==1){
		print(noquote(sprintf("Progresso: Simulacao %d de %d", i,nsim)))
	}
}

f.matrizRF<-function(P,irredutiveis,reordenar=FALSE){
	n<-nrow(P)
	estados<-1:n
	nconjuntos<-length(irredutiveis)
	R<-matrix(0,n,n)
	F<-matrix(0,n,n)
	recorrentes<-c()
	nr<-0
	j<-0
	for(ic in 1:nconjuntos){
		conjunto<-irredutiveis[[ic]]
		for(i in conjunto){
			nr<-nr+1
			recorrentes[nr]<-i
			for(j in conjunto){
				R[i,j]<-Inf
				F[i,j]<-1
			}
		}
	}
	transientes<-c()
	j<-0
	for(i in 1:n){
		if(sum(i==recorrentes)==0){
			j<-j+1
			transientes[j]<-i
		}
	}
	PN<-P%^%1000
	#browser()
	for(it in transientes){
			for(ic in 1:nconjuntos){
			conjunto<-irredutiveis[[ic]]
			prob<-0
			for(ir in conjunto){
				prob<-prob+PN[it,ir]
			}
			for(ir in conjunto){
				if(prob>1e-8){
					R[it,ir]<-Inf
				}else{
					R[it,ir]<-0
				}
			}				
		}
	}
	nq<-length(transientes)
	Q<-matrix(0,nq,nq)
	j<-0
	for(i in transientes){
		j<-j+1
		Q[j,]<-P[i,transientes]
	}
	I<-diag(nq)
	RT<-solve(I-Q)
	R[transientes,transientes]<-RT
	
	#Calculando F de transiente para transiente
	for(i in transientes){
		for(j in transientes){
			if(i==j){
				F[i,j]<-1-1/R[j,j]
			}else{
				F[i,j]<-R[i,j]/R[j,j]
			}
		}
	}
	
	#Calculando F de transiente para recorrente
	for(ic in 1:nconjuntos){
		conjunto<-irredutiveis[[ic]]
		b<-matrix(0,nq,1)
		i<-0
		for(it in transientes){
			i<-i+1
			b[i,1]<-sum(P[it,conjunto])
		}
		f<-RT%*%b
		F[transientes,conjunto]<-f
	}	
	
	#Reordenando, se solicitado
	if(reordenar){
		index<-c(recorrentes,transientes)
		R<-R[index,index]
		F<-F[index,index]
		estados<-estados[index]
	}
	
	output<-list(R=R,F=F,estados=estados)
	
	return(output)
}