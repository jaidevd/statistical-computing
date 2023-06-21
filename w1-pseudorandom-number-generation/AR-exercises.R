# Negative binomial(r, p) from geometric(p)
# Say r = 3, p = 0.4

r <- 3
p <- 0.4
paramC <- 1 #(p ^ (r - 1)) / ((1 - p) ^ r) * choose(2 * r - 2, r - 1)


nSamples <- 1000

Z <- numeric(nSamples)
N_TRIES <- rep(NA, nSamples)
for (ix in 1:nSamples) {
  n_tries <- 0
  while (TRUE) {
    n_tries = n_tries + 1
    proposal <- rgeom(1, prob=p)
    p_Proposal <- dgeom(proposal, prob=p)
    p_Target <- dnbinom(proposal, size=r, prob=p)
    if (runif(1) < (p_Target / (paramC * p_Proposal))) {
      break
    }
  }
  Z[ix] <- proposal
  N_TRIES[ix] <- n_tries
}

print(mean(N_TRIES))