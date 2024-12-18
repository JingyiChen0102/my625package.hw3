library(devtools)
library(testthat)
library(my625package.hw3)

test_that("my_lm produces correct results", {
  data(mtcars)
  X <- as.matrix(mtcars[, c("wt", "hp")])
  y <- mtcars$mpg

  # using my_lm function
  my_result <- my_lm(X, y)
  # using original R function:lm()
  lm_result <- lm(mpg ~ wt + hp, data = mtcars)

  # compare the coefficients
  expect_true(all.equal(as.numeric(my_result$coefficients),
                        as.numeric(coef(lm_result))))
})
