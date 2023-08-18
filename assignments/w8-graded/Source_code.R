# 1.(b) **R functions for first and second derivatives of the log-likelihood.**
f.prime <- function(data, x.m, alpha) {
  length(data) / alpha + length(data) * log(x.m) - sum(log(data))
}
f.double.prime <- function(data, x.m, alpha) {
  -length(data) / (alpha ^ 2)
}


# 1.(d) **Newton-Raphson on the given data.**
newton.raph <- function(data, x.m, tol=1e-5) {
  guess <- mean(data)
  guesses <- c(guess)
  first.d <- f.prime(data,  x.m, guess)
  while(abs(first.d) > tol) {
    first.d <- f.prime(data, x.m, guess)
    second.d <- f.double.prime(data, x.m, guess)
    guess <- guess - first.d / second.d
    guesses <- append(guesses, guess)  # Append the guess
  }
  return(guesses)
}

# 1.(e) **For $x_m = 2$, find the optimum value.**
X <- c(2.111398, 2.354323, 2.339421, 2.160081, 3.362930, 2.182222, 2.096592,
       2.699267, 3.789588, 2.421125)
alpha.guesses <- newton.raph(X, x.m=2)
sprintf("Optimal alpha: %f", tail(alpha.guesses, 1))

# 1.(f) **Plot the log-likelihood function and the values from Newton-Raphson.**
pareto.ll <- function(data, alpha, x.m) {
  logsum <- sum(log(data))
  n <- length(data)
  return(n * log(alpha) + n * alpha * log(x.m) - (alpha + 1) * logsum)
}
alphas <- seq(1, 7, length.out=1000)
plot(alphas, pareto.ll(X, alphas, x.m=2), type="s",
     xlab=expression(alpha), ylab="Log Likelihood")
lines(alpha.guesses, pareto.ll(X, alpha.guesses, x.m=2), type="p",
      col="blue", pch=15,cex=1)
abline(v=mean(X), col="red", lty=2)
abline(v=tail(alpha.guesses, 1), col="green", lty=2)
legend("bottomright", lty=2, col=c("red", "green"),
       legend=c("Initial guess (mean)", "Optimal alpha"))


# 2.(a) **Load data, add intercept to $X$ and encode the target variable.**
require("mlbench")
data("PimaIndiansDiabetes")
df <- PimaIndiansDiabetes
y <- as.integer(df$diabetes == "pos")
X <- df[, !(names(df) %in% c("diabetes"))]
X <- as.matrix(cbind(X.intercept = 1, X))

# 2.(b) **Define a function `beta.new` to perform NR on the dataset.**
beta.new <- function(X, y, tol=1e-10) {
    
  p <- ncol(X)
  beta <- matrix(0, nrow=p, ncol=1)
  
  n.iter <- 0
  jac.norm <- 10000  # Some arbitrary high value
  jac.norms <- c()
  
  while (jac.norm > tol) {
    n.iter <- n.iter + 1
    jacobian <- colSums(X * as.numeric(y - 1 / (1 + exp(-X %*% beta))))
    jac.norm <- norm(as.matrix(jacobian), type="F")
    jac.norms <- append(jac.norms, jac.norm)
    
    w.diag <- as.numeric(exp(X %*% beta) / (1 + exp(X %*% beta)) ^ 2)
    W <- diag(w.diag)
    hessian <- -t(X) %*% W %*% X
    
    beta <- beta - solve(hessian) %*% jacobian
  }
  return(list(beta=beta, n.iter=n.iter, norms=jac.norms))
}
nr.result <- beta.new(X, y)

# 2.(c) **Number of iterations needed for NR to converge.**
print(nr.result$n.iter)

# 2.(d) **What is the probability that the woman has diabetes?**
sample <- c(1, 4, 155, 40, 15, 55, 39.1, 0.506, 41)
p.diabetes <- 1 / (1 + exp(-sample %*% nr.result$beta ))
print(p.diabetes)

# 2.(e). **Plot norm of the gradient wrt the number of iterations.**
plot(nr.result$norms, type="o", main="Newtown-Raphson Convergence",
 ylab="Norm of the Jacobian", xlab="Iteration")
