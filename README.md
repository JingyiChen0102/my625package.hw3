# my625package.hw3
The goal of my625package.hw3 is to provides an optimized implementation of linear regression using matrix operations. It is designed to be more efficiency than the base R `lm()` function for small to moderately sized datasets. The package also includes features for computing key regression statistics such as R-squared, adjusted R-squared, residuals, and F-statistics.

## Key Features
- Efficient computation of regression coefficients using matrix algebra.
- Detailed output, including:
  - Coefficients
  - Fitted values
  - Residuals
  - R-squared and adjusted R-squared
  - Residual summary statistics
  - F-statistic
- Custom implementation ensures more efficiency compared to base R `lm()`.

## Installation
You can install the development version of my625package.hw3 from [GitHub](https://github.com/) with:

``` r
# Install devtools if not already installed
install.packages("devtools")

# Install the package
devtools::install_github("JingyiChen0102/my625package.hw3")
```

## Example
Below is an example of how to use the my_lm function with the built-in mtcars dataset:
```{r}
library(my625package.hw3)
data(mtcars)
X <- as.matrix(mtcars[, c("wt", "hp")])  # Predictor variables
y <- mtcars$mpg                          # Response variable
result <- my_lm(X, y)
result
```

## Output
my_lm function returns a list with the following components:
- coefficients: The estimated regression coefficients.
- fitted.values: The predicted values based on the model.
- residuals: The differences between observed and predicted values.
- r.squared: Proportion of variance explained by the model.
- adj.r.squared: Adjusted R-squared accounting for the number of predictors.
- sigma: Standard error of residuals.
- f.statistic: F-test statistic for the overall model fit.
- df: Degrees of freedom for regression and residuals.

## Benchmarking Performance
This is designed to test the efficiency of the implemented functions.

```{r}
bench::mark(
  my_lm = as.numeric(my_lm(X, y)$coefficients),
  lm = as.numeric(coef(lm(mpg ~ wt + hp, data = mtcars)))
)
```

## Contribution
Contributions are welcome! If you find a bug or have suggestions for improvements, feel free to create an issue or submit a pull request on the GitHub repository.
