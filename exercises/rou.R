samplerect <- function(a, b, c, d) {
  u <- runif(1, min=a, max=b)
  v <- runif(1, min=c, max=d)
  return(c(u, v))
}

sqrt.fx <- function(x) 1 / x

rou.sample <- function(a, b, c, d) {
  try <- 0
  while (TRUE) {
    try <- try + 1
    x <- samplerect(a, b, c, d)
    u <- x[1]
    v <- x[2]
    ratio <- v / u
    if (u <= sqrt(ratio)) {
      break
    }
  }
  return(c(ratio, try))
}

N <- 10000
samples <- numeric(N)
tries <- numeric(N)
for (i in 1:N) {
  sample <- rou.sample(0, 1, 0, 1)
  samples[i] <- sample[1]
  tries[i] <- sample[2]
}