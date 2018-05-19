#include <Rcpp.h>
using namespace Rcpp;

// Lagged Differences
// [[Rcpp::export]]
NumericVector diff_cpp(NumericVector x, int lag, int differences) {
  if (lag < 1 || differences < 1) {
    stop("`lag` or `differences` must be positive integers.");
  }

  int n = x.size();
  if (lag * differences >= n) {
    return(x[-1]);
  }

  NumericVector lag_x(n);
  NumericVector y(n);

  for (int i = 0; i < n; i++) {
    if (i < lag * differences) {
      lag_x[i] = NA_REAL;
    } else {
      lag_x[i] = x[i - lag];
    }
  }

  // first difference
  y = x - lag_x; 

  for (int j = 1; j < differences; j++) {
    y = diff(y);
  }

  return y;
}
