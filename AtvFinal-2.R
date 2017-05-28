# exercicio 9.2

t.arrive = 12

t.in = numeric(10)
t.out = numeric(10)
N.system = numeric(100)

next.out = 0
next.in = 0
t.clock = 0
N.in = 0

while(t.clock < 100){
  next.in = next.in + t.arrive
  is.scrap = sample(1:2, size = 1, replace = TRUE, c(0.25, 0.75))
  if(is.scrap == 2){
    N.in = N.in + 1
    next.out = t.clock + sample(c(17, 16, 15, 14), size = 1, replace = TRUE, c(0.25, 0.5, 0.125, 0.125))
    t.in[N.in] = t.clock
    t.out[N.in] = next.out
  }
  t.clock = next.in
}

print(list(t.in, t.out))

for(i in 1:100){
  N.system[i] = 0
  for(j in 1:N.in){
    if(t.in[j] <= i & t.out[j] >= i)
      N.system[i] = N.system[i] + 1
  }
}

print(mean(N.system))
print(N.system)