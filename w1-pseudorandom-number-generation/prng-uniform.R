mixed_congruential <- function(
    seed, n_samples, a = 7^5, m = (2 ^ 31) - 1, c=(2 ^ 10) - 1) {
  x <- rep(NA, times=n_samples)
  x[1] <- seed
  hasRepeat <- FALSE
  for (i in 2:n_samples) {
    val <- (x[i - 1] * a + c) %% m
    # If val is already present, the values will repeat
    if (val %in% x[1:i]) {
      hasRepeat <- TRUE
      break
    }
    x[i] <- val
  }
  if (hasRepeat) {  # loop broke because of repeating values, so fill the rest
    reps <- x[1:(i - 1)]  # the original sequence to be repeated
    x[i:n_samples] <- rep(reps, length.out=(n_samples - i + 1))
  }
  return(x / m)
}


uniform_dist <- function(seed, n_samples, a, b) {
  x <- mixed_congruential(seed, n_samples)
  return((b - a) * x + a)
}


# Usage: Draw 1k samples ~ U(3, 7)
y <- uniform_dist(0, 1000, 3, 7)
par(mfrow=c(1, 2))
hist(y, main="", xlab="", sub="Histogram", font.sub=1)
plot.ts(y, main="", xlab="", sub="Trace", font.sub=1)
mtext("Multiplicative Congruential Method: 1000 samples, seed = 0", outer=TRUE,
      side=3, line=-2, cex=1.5)