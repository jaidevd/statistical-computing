misc.poly <- function(a, b) {
  nTry <- 0
  cMax <- 1.5 * (b - a)
  pprop <- 1 / (b - a)
  while (TRUE) {
    nTry <- nTry + 1
    proposal <- runif(1, a, b)
    ptarget <- 6 * proposal * (1 - proposal)
    if (runif(1) <= ptarget / (cMax * pprop)) {
      break
    }
  }
  return(c(proposal, nTry))
}

N <- 10000
X <- numeric(N)
TRY <- numeric(N)
for (i in 1:N) {
  sample <- misc.poly(0, 1)
  X[i] <- sample[1]
  TRY[i] <- sample[2]
}

print(N / sum(TRY))
plot(density(X))
print(mean(X))