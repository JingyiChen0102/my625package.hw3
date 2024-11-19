library(devtools)
library(testthat)
library(my625package.hw3)

test_that("residual4 calculates all residual types correctly", {
  data(mtcars)
  model <- lm(mpg ~ wt + hp, data = mtcars)
  residuals <- residuals4(model)

  # test raw residuals
  expect_equal(residuals$raw_residuals, resid(model))

  # test standardized residuals
  mse <- sum(residuals$raw_residuals^2) / (nrow(model.matrix(model)) - ncol(model.matrix(model)))
  expected_std_residuals <- residuals$raw_residuals / sqrt(mse)
  expect_equal(residuals$std_residuals, expected_std_residuals)

  # test internally studentized residuals
  X <- model.matrix(model)
  hat_matrix <- X %*% solve(t(X) %*% X) %*% t(X)
  leverage <- diag(hat_matrix)
  expected_int_student_residuals <- residuals$raw_residuals / sqrt(mse * (1 - leverage))
  expect_equal(residuals$int_student_residuals, expected_int_student_residuals)

  # test externally studentized residuals
  n <- nrow(X)
  p <- ncol(X)
  expected_ext_student_residuals <- residuals$raw_residuals /
    sqrt((mse * (1 - leverage)) * (n - p - 1) / (n - leverage - 1))
  expect_equal(residuals$ext_student_residuals, expected_ext_student_residuals)
})

test_that("calculate_residuals output structure is correct", {
  data(mtcars)
  model <- lm(mpg ~ wt + hp, data = mtcars)
  residuals <- residuals4(model)
  expect_length(residuals, 4)
  expect_named(residuals, c("raw_residuals", "std_residuals", "int_student_residuals", "ext_student_residuals"))
})

