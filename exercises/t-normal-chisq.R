box.mueller <- function() {
  u1 <- runif(1)
  u2 <- runif(1)
  r <- sqrt(-2 * log(u1))
  theta <- 2 * pi * u2
  return(c(r * cos(theta), r * sin(theta)))
}

chi.sq <- function(k) {
  total <- 0
  for (i in 1:floor(k / 2)) {
    total <- total + sum(box.mueller() ^ 2)
  }
  if (k %% 2 == 1) {
    total <- total + box.mueller()[1] ^ 2
  }
  return(total)
}

t.dist <- function(k) {
  CHI <- chi.sq(k)
  Z <- box.mueller()[1]
  return(Z / sqrt(CHI / k))
}

N <- 1000
X <- numeric(N)
for (i in 1:N) {
  X[i] <- t.dist(5)
}

plot(density(X))
sup <- seq(-4, 4, length.out=N)
lines(sup, dt(sup, 5))