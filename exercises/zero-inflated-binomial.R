zif.binom <- function(N, delta, n, p) {
  X <- numeric(N)
  for (i in 1:N) {
    u <- runif(1)
    if (u < delta) {
      X[i] <- 0
    } else { X[i] <- rbinom(1, size=n, prob=p) }
  }
  return(X)
}

N <- 1000000
n <- 100
p <- 0.6
delta <- 0.3

X <- zif.binom(N, delta, n, p)
print(sum(X == 0) / N)
print(delta + (1 - delta) * n * p * ((1-p)^n))
print(mean(X))
print((1 - delta) * n * p)