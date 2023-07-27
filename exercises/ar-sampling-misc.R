fx <- function(x, a, b) {
  num <- (x ^ (a - 1)) * ((1 - x) ^ (b - 1))
  return(num / beta(a, b))
}

cmax <- function(a, b) {
  xopt <- (1 - a) / (2 - a - b)
  val <- 2 / beta(a, b) * (xopt ^ (a - 1)) * ((1 - xopt) ^ (b - 1))
  return(val)
}

N <- 1000
x <- seq(0, 1, length.out=N)
a <- 2
b <- 3
X <- fx(x, a, b)
# plot(density(X))


C <- cmax(a, b)
Z <- numeric(N)
nTry <- numeric(N)
for (i in 1:N) {
  try <- 0
  while (TRUE) {
    try <- try + 1
    proposal <- runif(1, min=-1, max=1)
    
    pTarget <- fx(proposal, a, b)
    pPropos <- 0.5
    
    if (runif(1) <= (pTarget / (C * pPropos))) {
      break
    }  
  }
  Z[i] <- proposal
  nTry[i] <- try
}
  