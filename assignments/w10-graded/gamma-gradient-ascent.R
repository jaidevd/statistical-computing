require("latex2exp")

log.like <- function(X, alpha) {
  n <- length(X)
  return((alpha - 1) * sum(log(X)) - sum(X) - n * log(gamma(alpha)))
}

f.prime <- function(X, alpha) {
  n <- length(X)
  return (sum(log(X)) - n * digamma(alpha))
}

gradient.ascent <- function(X, alpha0, t=0.1, tol=1e-5, return.guesses=TRUE) {
  guess <- alpha0
  grad <- f.prime(X, guess)
  guesses <- c(guess)
  
  while (abs(grad) >= tol) {
    guess <- guess + t * grad
    guesses <- cbind(guesses, guess)
    grad <- f.prime(X, guess)
  }
  if (return.guesses) { return(guesses) }
  return(guess)
}

alpha.star <- 10
N <- 100
X <- rgamma(N, alpha.star, rate=1)

alphas <- seq(1, 40, length.out=1000)
L_alpha <- log.like(X, alphas)

alpha_opt <- gradient.ascent(X, 20)
alpha_opt2 <- gradient.ascent(X, mean(X))
alpha_opt3 <- gradient.ascent(X, 35)

plot(alphas, L_alpha, type='l')
points(alpha_opt, log.like(X, alpha_opt), col=rgb(1, 0, 0, alpha=0.7), pch=19)
points(alpha_opt2, log.like(X, alpha_opt2), col=rgb(0, 1, 0, alpha=0.7), pch=19)
points(alpha_opt3, log.like(X, alpha_opt3), col=rgb(0, 0, 1, alpha=0.7), pch=19)

legend("topright", legend = c(
  TeX(r'($\alpha_0 = 20$)'),
  TeX(r'($\alpha_0 = \bar{X}$)'),
  TeX(r'($\alpha_0 = 35$)')
  ), pch = 19,
  col = c(rgb(1, 0, 0, alpha=0.7), rgb(0, 1, 0, alpha=0.7), rgb(0, 0, 1, alpha=0.7))
)