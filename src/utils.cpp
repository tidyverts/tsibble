#include <Rcpp.h>
using namespace Rcpp;
using namespace std;

int gcd(int x, int y) {
  return y == 0 ? x : gcd(y, x % y);
}

// Find the greatest common divisor for a vector of numerics
// [[Rcpp::export]]
int gcd_vector(NumericVector x) {
  NumericVector abs_diff = abs(diff(x));

  return std::accumulate(abs_diff.begin(), abs_diff.end(), abs_diff[0], gcd);
}

// Equivalent to any(x != c)

// [[Rcpp::export]]
bool any_not_equal_to_c(NumericVector x, double c) {
  NumericVector::iterator it;

  for (it = x.begin(); it != x.end(); ++it) {
    if (*it != c) {
      return true;
    }
  };
  return false;
}

// If the input x is in ascending order

// [[Rcpp::export]]
bool is_ascending(IntegerVector x) {
  IntegerVector y = wrap(na_omit(x));
  int prev = y[0];

  IntegerVector::iterator it;
  for (it = y.begin() + 1; it != y.end(); ++it) {
    if (prev < *it) {
      prev = *it;
    } else {
      return false;
    }
  };
  return true;
}
