sample.normal <- function() {
  U <- runif(2)
  r2 <- -2 * log(U[1])
  theta <- 2 * pi * U[2]
  x <- sqrt(r2) * cos(theta)
  y <- sqrt(r2) * sin(theta)
  y <- sqrt(r2) * sin(theta)
  return(c(x, y))
}

x <- numeric(1000)
for (i in 0:499) {
  samples <- sample.normal()
  x[i * 2 + 1] <- samples[1]
  x[i * 2 + 2] <- samples[2]
}

chi.sq.2 <- function() { sum(sample.normal() ^ 2) }

N <- 1000
Z <- numeric(N)
for (i in 1:N) {
  Z[i] <- chi.sq.2()
}
plot(density(Z))