library(pdq)

Ta <- 1.3333
Ts <- 1.0000

Init("")
CreateOpen("w", 1 / Ta)
CreateNode("n", CEN, FCFS)
SetDemand("n", "w", Ts)
Solve(CANON)

R <- GetResidenceTime("n", "w", TRANS)
Q <- GetQueueLength("n", "w", TRANS)
U <- GetUtilization("n", "w", TRANS)
X <- GetThruput(TRANS, "w")

print(U)
print(X)
print(R)
print(Q)