# Examples of generating a Bernoulli, a Binomial and a Geometric distribution
# from a Uniform distribution, using the inverse transform sampling method.


unif_bernoulli <- function(n_samples, p) {
  x <- runif(n_samples)
  return(as.numeric(x < p))
}

unif_binomial <- function(n_samples, n, p) {
  x <- runif(n_samples)
  cdf <- unique(pbinom(-1:n, n, p))
  return(cut(x, cdf, labels=FALSE) - 1)
}

unif_geom <- function(n_samples, p) {
  x <- runif(n_samples)
  cdf <- unique(pgeom(1:n_samples, p))
  return(cut(x, cdf, labels=FALSE))
}
