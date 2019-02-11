#include <Rcpp.h>
using namespace Rcpp;
using namespace std;

// Run length ecoding
// ref: https://github.com/hadley/adv-r/blob/master/extras/cpp/rle.cpp
// [[Rcpp::export]]
List rle_lgl(NumericVector x) {
  std::vector<int> lengths;
  std::vector<bool> values;

  // Initialise first value
  int i = 0;
  double prev = x[0];
  values.push_back(prev);
  lengths.push_back(1);

  for(NumericVector::iterator it = x.begin() + 1; it != x.end(); ++it) {
    if (prev == *it) {
      // Same as previous so increment lengths
      lengths[i]++;
    } else {
      // Different, so add to values, and add 1 to lengths
      values.push_back(*it);
      lengths.push_back(1);

      i++;
      prev = *it;
    }
  }

  return List::create(_["lengths"] = lengths, _["values"] = values);
}

int gcd(int x, int y) {
  return y == 0 ? x : gcd(y, x % y);
}

// Find the greatest common divisor for a vector of numerics
// [[Rcpp::export]]
int gcd_vector(NumericVector x) {
  NumericVector abs_diff = unique(abs(diff(x)));

  return std::accumulate(abs_diff.begin(), abs_diff.end(), abs_diff[0], gcd);
}

// Equivalent to any(x != c)

// [[Rcpp::export]]
bool any_not_equal_to_c(NumericVector x, double c) {
  return is_true(any(x != c));
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
