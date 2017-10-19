#include <Rcpp.h>
using namespace Rcpp;

// Find the lowest positive value

// [[Rcpp::export]]
double minp(NumericVector x) {
  NumericVector::iterator it;
  double z = 0;

  for(it = x.begin(); it != x.end(); ++it) {
    if (*it > 0 && (*it < z || z == 0)) {
      z = *it;
    }
  }

  return z;
}

// Equivalent to any(x != c)

// [[Rcpp::export]]
bool any_not_equal_to_c(NumericVector x, double c) {
  NumericVector::iterator it;

  for(it = x.begin(); it != x.end(); ++it) {
    if (*it != c) {
      return true;
    }
  };
  return false;
}

// Sliding window function

// [[Rcpp::export]]
std::vector<double> slide_cpp(NumericVector x, Function f, int size, double fill) {
  int n = x.size();
  NumericVector y(size);
  List z(n);

  int na_obs = size - 1;
  int z_n = n - na_obs;
  for (int i = 0; i < n; ++i) {
    if (i >= z_n) {
      z[i] = fill;
    } else {
      for (int j = 0; j < size; ++j) {
        y[j] = x[i + j];
      }
      z[i] = f(y);
    }
  }

  std::vector<double> output(z.begin(), z.end());
  return(output);
}
