binom.pdf <- function(n, p) {
  support <- 0:n
  pdf <- choose(n, support) * (p ^ support) * ((1 - p) ^ (n - support))
  return(pdf)
}

binom.cdf <- function(n, p) {
  support <- 0:n
  cdf <- numeric(length(support))
  cdf[1] <- choose(n, 0) * ((1 - p) ^ n)
  for (i in 1:n) {
    cdf[i + 1] <- cdf[i] + choose(n, i) * (p ^ i) * ((1 - p) ^ (n - i))
  }
  return(cdf)
}

inv.transform <- function(N, n, p) {
  support <- -1:n
  cdf <- pbinom(support, size=n, prob=p)
  samples <- numeric(N)
  for (i in 1:N) {
    u <- runif(1)
    samples[i] <- cut(u, cdf, labels=FALSE) - 1
  }
  return(samples)
}

X <- inv.transform(200, n, p)