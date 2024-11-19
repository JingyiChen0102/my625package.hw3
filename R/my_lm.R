#' Linear Regression Model
#'
#' Implements linear regression using efficient matrix operations, providing detailed output.
#'
#' @param X A matrix of predictor variables.
#' @param y A numeric vector of response variables.
#' @return A list with components including coefficients, fitted values, residuals, R-squared,
#'         adjusted R-squared, standard error, F-statistic, degrees of freedom, and residual summary.
#' @examples
#' data(mtcars)
#' X <- as.matrix(mtcars[, c("wt", "hp")])
#' y <- mtcars$mpg
#' result <- fastlm(X, y)
#' print(result)
#' @export
my_lm <- function(X, y) {
  if (!is.matrix(X)) stop("X must be a matrix.")
  if (!is.numeric(y)) stop("y must be numeric.")

  if (!all(X[, 1] == 1)) {
    X <- cbind(1, X)
  }

  X <- as.matrix(X)
  y <- as.matrix(y)
  n <- nrow(X)
  p <- ncol(X)

  XtX <- crossprod(X)
  Xty <- crossprod(X, y)
  beta <- solve(XtX, Xty)

  fitted <- X %*% beta
  residuals <- y - fitted
  rss <- sum(residuals^2)
  tss <- sum((y - mean(y))^2)
  regss <- tss - rss

  r.squared <- regss / tss
  adj.r.squared <- 1 - (rss / (n - p)) / (tss / (n - 1))
  sigma <- sqrt(rss / (n - p))
  f.statistic <- (regss / (p - 1)) / (rss / (n - p))

  list(
    coefficients = as.vector(beta),
    fitted.values = as.vector(fitted),
    residuals = as.vector(residuals),
    r.squared = r.squared,
    adj.r.squared = adj.r.squared,
    sigma = sigma,
    f.statistic = f.statistic,
    df = c(p - 1, n - p),
    residuals.summary = summary(residuals),
    call = match.call()
  )
}
