#include <Rcpp.h>
using namespace Rcpp;

// Cholesky decomposition to solve XtX * beta = Xty
NumericVector choleskySolve(const NumericMatrix& XtX, const NumericVector& Xty) {
  int p = XtX.nrow();
  NumericMatrix L(p, p); // Lower triangular matrix
  NumericVector y(p);
  NumericVector beta(p);

  // Cholesky Decomposition (XtX = L * L^T)
  for (int i = 0; i < p; ++i) {
    for (int j = 0; j <= i; ++j) {
      double sum = 0.0;
      for (int k = 0; k < j; ++k) {
        sum += L(i, k) * L(j, k);
      }
      if (i == j) {
        L(i, j) = sqrt(XtX(i, j) - sum);
    } else {
        L(i, j) = (XtX(i, j) - sum) / L(j, j);
      }
    }
  }

  // Solve L * y = Xty using forward substitution
  for (int i = 0; i < p; ++i) {
    double sum = 0.0;
    for (int j = 0; j < i; ++j) {
      sum += L(i, j) * y[j];
   }
    y[i] = (Xty[i] - sum) / L(i, i);
  }

  // Solve L^T * beta = y using backward substitution
  for (int i = p - 1; i >= 0; --i) {
    double sum = 0.0;
    for (int j = i + 1; j < p; ++j) {
      sum += L(j, i) * beta[j];
   }
    beta[i] = (y[i] - sum) / L(i, i);
 }

  return beta;
}

// [[Rcpp::export]]
NumericVector rcpp_lm(NumericMatrix X, NumericVector y) {
  int n = X.nrow();
  int p = X.ncol();

  // Initialize XtX and Xty
  NumericMatrix XtX(p, p);
  NumericVector Xty(p);

  // Calculate XtX and Xty
  for (int i = 0; i < p; ++i) {
    for (int j = 0; j < p; ++j) {
      XtX(i, j) = 0.0;
      for (int k = 0; k < n; ++k) {
        XtX(i, j) += X(k, i) * X(k, j);
      }
  }
    Xty[i] = 0.0;
    for (int k = 0; k < n; ++k) {
      Xty[i] += X(k, i) * y[k];
    }
  }

  // Solve for beta using Cholesky decomposition
  NumericVector beta = choleskySolve(XtX, Xty);

  return beta;
}
