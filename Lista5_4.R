t.end <- 10^5
t.clock <- 0
Ta <- 1.3333
Ts <- 1.0000
t1 <- 0
t2 <- t.end
tn <- t.clock
tb <- 0
n <- 0
s <- 0
b <- 0
c <- 0
qc <- 0
tc <- 0
plotSamples <- 100
set.seed(1)

while (t.clock < t.end){
  if (t1 < t2){
    t.clock <- t1
    s <- s + n * (t.clock - tn)
    n <- n + 1
    if (t.clock < plotSamples){
      qc <- append(qc, n)
      tc <- append(tc, t.clock)
    }
    tn <- t.clock
    t1 <- t.clock + rexp(1, 1/Ta)
    if (n == 1){
      tb <- t.clock
      t2 <- t.clock + rexp(1, 1/Ts)
    }
  }else{
    t.clock <- t2
    s <- s + n * (t.clock - tn)
    n <- n - 1
    if (t.clock < plotSamples){
      qc <- append(qc, n)
      tc <- append(tc, t.clock)
    }
    tn <- t.clock
    c <- c + 1
    if (n > 0){
      t2 <- t.clock + rexp(1, 1/Ts)
    }else{
      t2 <- t.end
      b <- b + t.clock - tb
    }
  }
}

u <- b / t.clock
N <- s / t.clock
x <- c / t.clock
r <- N / x
q <- sum(qc) / max(tc)

print(u)
print(x)
print(r)
print(q)

plot(qc, type = 'S', xlim = c(0, plotSamples), ylim = c(0, 6), xlab = NA, ylab = NA)
lines(rep(1.7845, 153), type = 'S', col = "red")
title("M/M/1 Simulation", xlab = "Time", ylab = "Instantaneous queue length")