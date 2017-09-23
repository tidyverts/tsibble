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
bool any_c(NumericVector x, double c) {
  int n = x.size();

  for(int i = 0; i < n; ++i) {
    if (x[i] != c) {
      return true;
    }
  };
  return false;
}
