require("mlbench")

df <- PimaIndiansDiabetes
y <- as.integer(df$diabetes == "pos")
X <- df[, !(names(df) %in% c("diabetes"))]
X <- as.matrix(cbind(X.intercept = 1, X))

multivariate.newton.raph <- function(X, y, tol=1e-10) {
  
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


coeff <- multivariate.newton.raph(X, y)
sample <- c(1, 4, 155, 40, 15, 55, 39.1, 0.506, 41)
print(1 / (1 + exp(-sample %*% coeff$beta )))

plot(coeff$norms, type="o", main="Newtown-Raphson Convergence",
     ylab="Norm of the Jacobian", xlab="Iteration")