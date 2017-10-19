#include <Rcpp.h>
using namespace Rcpp;

// Find the lowest positive value

// [[Rcpp::export]]
double minp(NumericVector x) {
  int n = x.size();
  double z = 0;

  for(int i = 0; i < n; ++i) {
    if (x[i] > 0 && (x[i] < z || z == 0)) {
      z = x[i];
    }
  }

  return z;
}

// Equivalent to any(x != c)

// [[Rcpp::export]]
bool any_not_equal_to_c(NumericVector x, double c) {
  int n = x.size();

  for(int i = 0; i < n; ++i) {
    if (x[i] != c) {
      return true;
    }
  };
  return false;
}

// Sliding window function

// [[Rcpp::export]]
List slide_cpp(NumericVector x, Function f, int size) {
  int n = x.size();
  NumericVector y(size);
  List z(n);

  int na_obs = size - 1;
  for (int i = 0; i < n; ++i) {
    if (na_obs == 0) {
      z[i] = x[i];
    } else {
      if (i < na_obs) {
        z[i] = NA_REAL;
      } else {
        for (int j = 0; j < size; ++j) {
          y[j] = x[i + j - 1];
        }
        z[i] = f(y);
      }
    }
  }

  return(z);
}
