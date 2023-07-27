paramC <- function(n, p, lambda) {
  support <- 0:n
  xpdf <- dbinom(support, n, p)
  ypdf <- dpois(support, lambda)
  return(max(xpdf / ypdf))
}

n <- 20
p <- 0.25
lambda <- 10
N <- 1000

X <- numeric(N)
tries <- numeric(N)

cmax <- paramC(n, p, lambda)

for (i in 1:N) {
  try <- 0
  while (TRUE) {
    try <- try + 1
    proposal <- rpois(1, lambda=lambda)
    pTarget <- pbinom(proposal, size=n, prob=p)
    pProposal <- ppois(proposal, lambda=lambda)
    if (runif(1) <= (pTarget / (cmax * pProposal))) {
      break
    }
  }
  tries[i] <- try
  X[i] <- proposal
}