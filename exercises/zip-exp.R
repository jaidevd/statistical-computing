zip.exp <- function(N, lambda, delta) {
  samples <- numeric(N)
  # u <- runif(N)
  # samples[u < delta] <- 0
  # ix <- u >= delta
  # nRemain <- sum(ix)
  # samples[ix] <- rexp(nRemain, rate = lambda)
  for (i in 1:N) {
    if (runif(1) < delta) {
      samples[i] <- 0
    } else { samples[i] <- rexp(1, rate=lambda)}
  }
  return(samples)
}

lambda <- 3
delta <- 0.1

X <- zip.exp(1000, lambda, delta)
hist(X)


mu <- (1 - delta) / lambda
print(mu)
print(mean(X))