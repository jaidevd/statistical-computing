inverse <- function(u, a, c) { exp(log(u) / a + log(c)) }

sample <- function(N, a, c) {
  unifs <- runif(N)
  return(inverse(unifs, a, c))
}

N = 1000
a = 5
c = 10
X <- sample(N, a, c)