#' Linear Regression with Rcpp
#'
#' Using Rcpp to improve my_lm function
#'
#' @param X A matrix of predictor variables.
#' @param y A numeric vector of response variables.
#' @return A list containing regression coefficients, fitted values, residuals, and statistics.
#' @example
#' data(mtcars)
#' X <- as.matrix(mtcars[, c("wt", "hp")])
#' y <- mtcars$mpg
#' result <- my_lm2(X, y)
#' result
#' @export
my_lm2 <- function(X, y) {
  if (!is.matrix(X)) stop("X must be a matrix.")
  if (!is.numeric(y)) stop("y must be numeric.")

  # Add intercept column if not present
  if (!all(X[, 1] == 1)) {
    X <- cbind(1, X)
  }

  X <- as.matrix(X)
  y <- as.numeric(y)  # Ensure y is a vector

  # Call Rcpp to compute coefficients
  beta <- rcpp_lm(X, y)

  # Calculate fitted values, residuals, and statistics in R
  fitted <- X %*% beta
  residuals <- y - fitted
  rss <- sum(residuals^2)
  tss <- sum((y - mean(y))^2)
  regss <- tss - rss

  r.squared <- regss / tss
  adj.r.squared <- 1 - (rss / (nrow(X) - ncol(X))) / (tss / (nrow(X) - 1))
  sigma <- sqrt(rss / (nrow(X) - ncol(X)))
  f.statistic <- (regss / (ncol(X) - 1)) / (rss / (nrow(X) - ncol(X)))

  # Return results
  list(
    coefficients = as.vector(beta),
    fitted.values = as.vector(fitted),
    residuals = as.vector(residuals),
    r.squared = r.squared,
    adj.r.squared = adj.r.squared,
    sigma = sigma,
    f.statistic = f.statistic,
    df = c(ncol(X) - 1, nrow(X) - ncol(X)),
    residuals.summary = summary(residuals),
    call = match.call()
  )
}
