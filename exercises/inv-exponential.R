inv.exp <- function(x, l) -log(1 - x) / l

exp.inv.unif <- function(N, l) {
  unifs <- runif(N)
  return(inv.exp(unifs, l))
}

N <- 1000
l <- 5

X <- exp.inv.unif(N, l)