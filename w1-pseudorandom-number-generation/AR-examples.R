# Try to sample a Binomial(n=4, p=0.25) distribution from a Geometric(p=0.25) distribution

paramC <- 2.25
nSamples <- 100000

pBinom <- 0.25
pGeom <- 0.25
nBinom <- 4

Z <- numeric(nSamples)
N_TRIES <- rep(NA, nSamples)
for (ix in 1:nSamples) {
  n_tries <- 0
  while (TRUE) {
    n_tries = n_tries + 1
    proposal <- rgeom(1, prob=pGeom)
    p_Proposal <- dgeom(proposal, prob=pGeom)
    p_Target <- dbinom(proposal, size=nBinom, prob=pBinom)
    if (runif(1) < (p_Target / (paramC * p_Proposal))) {
      break
    }
  }
  Z[ix] <- proposal
  N_TRIES[ix] <- n_tries
}

print(mean(N_TRIES))