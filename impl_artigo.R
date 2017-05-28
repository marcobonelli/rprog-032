rm (list = ls(all = TRUE))

#função de retorno do custo do estado para um 'm' e 'r' determinado
csk <- function(m, r){

  #fração de itens conformes no estado I
  p1 <- 0.99
  
  #fração de itens conformes no estado II
  p2 <- 0.80
  
  #probabilidade de mudança do estado I para o estado II
  pii <- 0.0001
  
  #probabilidade de classificar um item conforme como não conforme
  alfa <- 0.01
  
  #probabilidade de classificar um item não conforme como conforme
  beta <- alfa
  
  #número de itens produzidos entre a sinalização de item não conforme e a parada do processo
  L <- 10

  #custo de inspeção
  c0 <- 0.25
  
  #custo de envio de um item não conforme
  c1 <- 20
  
  #custo de ajuste
  c2 <- 100
  
  #custo de eliminar um item conforme
  c3 <- 2
  
  #custo de eliminar um item não conforme
  c4 <- c3

  P <- matrix(0:0, 6, 6)
  ee <- numeric(6)
  nn <- numeric(6)
  ss <- numeric(6)

  #probabilidade de uma decisão correta sobre o julgamento do item inspecionado
  b.alfa <- pbinom(ceiling(0.5 * r) - 1, r, 1 - alfa)
  
  #probabilidade de uma decisão errada sobre o julgamento do item inspecionado
  b.beta <- 1 - pbinom(ceiling(0.5 * r) - 1, r, beta)

  #todos os itens serem produzidos no estado I, e serem considerados conformes
  P[1, 1] <- ((1 - pii)^m) * ((p1 * (1 - b.alfa)) + ((1 - p1) * b.beta))
  
  #todos os itens serem produzidos no estado I, e serem considerados não conformes
  P[1, 2] <- ((1 - pii)^m) * (p1 * b.alfa + (1 - p1) * (1 - b.beta))
  
  #mudança do estado I para o estado II, e serem considerados conformes
  P[1, 3] <- (1 - (1 - pii)^m) * (p2 * (1 - b.alfa) + (1 - p2) * b.beta)
  
  #mudança do estado I para o estado II, e serem considerados não conformes
  P[1, 4] <- (1 - ((1 - pii)^m)) * ((p2 * b.alfa + (1 - p2) * (1 - b.beta)))
  
  #todos os itens produzidos no estado II, e serem considerados conformes
  P[3, 5] <- p2 * (1 - b.alfa) + (1 - p2) * b.beta
  
  #todos os itens produzidos no estado II, e serem considerados não conformes
  P[3, 6] <- p2 * b.alfa + (1 - p2) * (1- b.beta)

  #igualdade de estados
  P[2, ] <- P[1, ]
  P[4, ] <- P[1, ]
  P[6, ] <- P[1, ]
  P[5, ] <- P[3, ]

  A = t(P) - diag(6)
  B = matrix(0:0, 6, 6)

  A[6, ] = 1
  B[6, ] = 1

  X = solve(A, B)
  
  #probabilidades estacionárias
  y = X[, 6]
  
  #ee -> custo de enviar itens defeituosos para o consumidor ou estágios posteriores do processo
  #nn -> custo relacionado ao descarte do item inspecionado
  #ss -> custo de reajuste e de descartar os itens que compõem o atraso

  #produzido no estado I e considerado conforme
  ee[1] <- c1 * (m - 1) * (1 - p1)
  nn[1] <- c3 * (((1 - p1) * b.beta) / (p1 * (1 - b.alfa) + (1 - p1) * b.beta)) + c4 * (((1 - b.alfa) * p1) / (p1 * (1 - b.alfa) + (1 - p1) * b.beta))
  ss[1] <- 0

  #produzido no estado I e considerado não conforme
  ee[2] <- ee[1]
  nn[2] <- c3 * (((1 - p1) * (1 - b.beta)) / (p1 * b.alfa + (1 - p1) * (1 - b.beta))) + c4 * ((p1 * b.alfa) / (p1 * b.alfa + (1 - p1) * (1 - b.beta)))
  ss[2] <- c2 + L * ((1 - pii)^L) * ((1 - p1) * c3 + p1 * c4) + (((1 - pii) * (1 - (1 - pii)^L) - ((1 - pii)^L) * pii * L) / pii) * ((1 - p1) * c3 + p1 * c4) + ((((1 - pii)^(L + 1)) + pii * (L + 1) - 1) / pii) * ((1 - p2) * c3 + p1 * c4)

  #processo em transição do estado I para o estado II e considerado conforme
  ee[3] <- c1 * (1 - p1) * (((1 - pii) * (1 - (1 - pii)^m) - pii * m * (1 - pii)^m) / (pii * (1 - (1 - pii)^m))) + c1 * (1 - p2) * ((pii * m + ((1 - pii)^m) - 1) / (pii * (1 - (1 - pii)^m)))
  nn[3] <- c3 * (((1 - p2) * b.beta) / (p2 * (1 - b.alfa) + (1 - p2) * b.beta)) + c4 * (((1 - b.alfa) * p2) / (p2 * (1 - b.alfa) + (1 - p2) * b.beta))
  ss[3] <- 0

  #processo em transição do estado I para o estado II e considerado não conforme
  ee[4] <- ee[3]
  nn[4] <- c3 * (((1 - p2) * (1 - b.beta)) / (p2 * b.alfa + (1 - p2) * (1 - b.beta))) + c4 * ((p2 * b.alfa) / (p2 * b.alfa + (1 - p2) * (1 - b.beta)))
  ss[4] <- c2 + L * (c3 * (1 -p2) + c4 * p2)

  #processo no estado II e considerado conforme
  ee[5] <- c1 * (m - 1) * (1 - p2)
  nn[5] <- nn[3]
  ss[5] <- 0

  #processo no estado ii e considerado não conforme
  ee[6] <- ee[5]
  nn[6] <- nn[4]
  ss[6] <- ss[4]

  tmp <- 0
  
  T <- matrix(c(2, 1, 4, 3, 6, 5), 3, 2)
  
  #calculo parcal do custo do estado
  for(i in 0:2)
    for(j in 0:1)
      tmp <- tmp + y[T[i + 1, j + 1]] * (ee[T[i + 1, j + 1]] + nn[T[i + 1, j + 1]] + ss[T[i + 1, j + 1]])

  #término e retorno do custo do estado
  return ((1 / (m - 1)) * (r * c0 + tmp))
}

result <- matrix(0:0, 6, 100)

#calculo do custo do estado para um 'm' e 'r' determinado
for(r in 1:6)
  for(m in 1:100)
    result[r, m] <- csk(m, r)

print(result)

#calculando mínimos locais
p.min <- matrix(0:0, 6, 100)
for(r in 1:6)
  for(m in 1:100)
    if(result[r, m] == min(result[r, ]))
      p.min[r, m] <- result[r, m]

#criando linhas do gráfico
plot(c(1:100), result[1, ], type = 'l', col = "orange", xlim = c(30, 100) ,ylim =  c(0.3, 0.5), xlab = NA, ylab = NA)
lines(c(1:100), result[2, ], type = 'l', col = "purple")
lines(c(1:100), result[3, ], type = 'l', col = "green")
lines(c(1:100), result[4, ], type = 'l', col = "black")
lines(c(1:100), result[5, ], type = 'l', col = "blue")
lines(c(1:100), result[6, ], type = 'l', col = "red")

#atribuindo pontos de mínimo local
points(c(1:100), p.min[1, ], col = "orange")
points(c(1:100), p.min[2, ], col = "purple")
points(c(1:100), p.min[3, ], col = "green")
points(c(1:100), p.min[4, ], col = "black")
points(c(1:100), p.min[5, ], col = "blue")
points(c(1:100), p.min[6, ], col = "red")

title("Grafico de custo medio (C) em funcao de m e r", xlab = "m", ylab = "Custo ($)")
legend(90, 0.4, c("r = 1, mº = 55", "r = 2, mº = 65", "r = 3, mº = 51", "r = 4, mº = 52", "r = 5, mº = 53", "r = 6, mº = 55", "otimo local", "otimo global"), col = c("orange", "purple", "green", "black", "blue", "red", "black", "black"), lty = c(1, 1, 1, 1, 1, 1, NA, NA) , pch = c(NA, NA, NA, NA, NA, NA, 1, 20), cex = 0.5)