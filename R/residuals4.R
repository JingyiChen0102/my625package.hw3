#' Calculate Residuals
#'
#' This function calculates raw residuals, standardized residuals, internally studentized residuals,
#' and externally studentized residuals for a fitted linear model.
#'
#' @param model A linear model object created using lm().
#'
#' @return A list containing raw residuals, standardized residuals, internally studentized residuals,
#'         and externally studentized residuals.
#' @export
#'
#' @examples
#' data(mydata)
#' model <- lm(y ~ X, data = mydata)
#' residuals <- residuals4(model)
#' print(residuals)
residuals4 <- function(model) {
  X <- model$X
  y <- model$y
  residuals <- model$residuals
  fitted <- model$fitted

  # residuals
  raw_residuals <- residuals

  # Standardized residuals
  mse <- sum(raw_residuals^2) / (nrow(X) - ncol(X))
  std_residuals <- raw_residuals / sqrt(mse)

  # Leverage values
  hat_matrix <- X %*% solve(t(X) %*% X) %*% t(X)
  leverage <- diag(hat_matrix)

  # Internally studentized residuals
  int_student_residuals <- raw_residuals / sqrt(mse * (1 - leverage))

  # Externally studentized residuals
  ext_student_residuals <- raw_residuals / sqrt((mse * (1 - leverage)) * (nrow(X) - ncol(X) - 1) / (nrow(X) - leverage - 1))

  list(
    raw_residuals = raw_residuals,
    std_residuals = std_residuals,
    int_student_residuals = int_student_residuals,
    ext_student_residuals = ext_student_residuals
  )
}
library(devtools)
library(usethis)
library(roxygen2)
