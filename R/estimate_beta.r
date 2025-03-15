#' Function to estimate beta
#' 
#'
#' @param X An n*k matrix of k variables where n>=1.
#' @param y A vector of n observations where n>1.
#'
#' @return Estimate beta for linear regression.
#' @export
#'
#' @examples
#' y <- iris[, "Sepal.Length"] |> as.matrix()
#' X <- iris[, c("Sepal.Width", "Petal.Length", "Petal.Width")] |> as.matrix()
#' estimate_beta(y, X)
estimate_beta <- function(y, X) {

  if (!is.matrix(X) && !is.vector(X)) {
    stop("X must be a matrix or a column vector.")
  }
  if (!is.matrix(y) && !is.vector(y)) {
    stop("y must be a numerical vector.")
  }

  X <- as.matrix(X)
  y <- as.matrix(y)

  if (nrow(X) != length(y)) {
    stop("The number of rows in X must match the length of y.")
  }
  
  xtx <- t(X) %*% X
  det_xtx <- det(t(X) %*% X)
  n <- ncol(t(X) %*% X)
  inv_xtx <- t(outer(1:n, 1:n, Vectorize(function(i, j) (-1)^(i+j) * det(xtx[-i, -j])))) / det_xtx
  beta_hat <- inv_xtx %*% (t(X) %*% y)
  
  return(beta_hat)
}