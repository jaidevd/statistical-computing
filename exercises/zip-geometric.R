zip.geometric <- function(N, p, delta) {
  X <- numeric(N)
  for (i in 1:N) {
    if (runif(1) <= delta) {
      sample <- 0
    } else {
      sample <- rgeom(1, p)
    }
    X[i] <- sample
  }
  return(X)
}
 
p <- 0.3
delta <- 0.1


X <- zip.geometric(1000, p, delta)
print((1 - delta) * (1 - p) / p)
print(mean(X))