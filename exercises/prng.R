mixed.congruential <- function(N, a, c, m, seed = 0) {
  samples <- numeric(length=N)
  samples[1] <- seed
  for (i in 2:N) {
    samples[i] <- (samples[i - 1] * a + c) %% m
  }
  return(samples)
}

X <- mixed.congruential(1000, 8 ^ 5, 2 ^ 8 - 1, 2^41 - 1, 10)
# hist(X)
plot.ts(X)