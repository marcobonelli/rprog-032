# exercicio 10.3

demand.men = 50
purchase.cost = 550
sell.price = 995
buy.back = 200

demand.vector = rpois(1000, demand.men)
profit = numeric(1000)
profit.mean = matrix(0:0, max(demand.vector), 1)

for(i in 1 : max(demand.vector)){
  for(j in 1 : 1000){
    profit[j] = (min(i, demand.vector[j]) * sell.price) - (purchase.cost * i) - (max(i - demand.vector[j], 0) * buy.back) 
  }
  profit.mean[i, 1] = mean(profit)
}

print(profit.mean)