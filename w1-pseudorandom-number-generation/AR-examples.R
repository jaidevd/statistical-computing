# Try to sample a Bernoulli(p=0.5) distribution from a Binomial(n=4, p=0.2) distribution

paramC <- 1.22
n_samples = 100
Z <- rep(NA, n_samples)
N_TRIES <- rep(0, n_samples)
for (sample_ix in 1:n_samples) {
  accept <- FALSE
  n_tries <- 0
  while (!accept) {
    n_tries <- n_tries + 1
    U <- runif(1)
    Y <- rbinom(1, size=4, p=0.2)
    py <- 0
    if (Y %in% c(0, 1)) {
      py <- 0.5
    }
    qy <- dbinom(Y, size=4, p=0.2)
    if (U <= (py / paramC * qy)) {
      accept = TRUE
    }
  }
  Z[sample_ix] <- Y
  N_TRIES[sample_ix] <- n_tries
}
