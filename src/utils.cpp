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
  NumericVector abs_diff = abs(diff(x));

  return std::accumulate(abs_diff.begin(), abs_diff.end(), abs_diff[0], gcd);
}

// Find the lowest positive value

// double minp(NumericVector x) {
//   NumericVector::iterator it;
//   double z = 0;
//
//   for (it = x.begin(); it != x.end(); ++it) {
//     if (*it > 0 && (*it < z || z == 0)) {
//       z = *it;
//     }
//   }
//
//   return z;
// }

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

// If the input x is in descending order

// [[Rcpp::export]]
bool is_descending(IntegerVector x) {
  int prev = x[0];

  IntegerVector::iterator it;
  for (it = x.begin() + 1; it != x.end(); ++it) {
    if (prev >= *it) {
      prev = *it;
    } else {
      return false;
    }
  };
  return true;
}

// If the input x is in ascending order

// [[Rcpp::export]]
bool is_ascending(IntegerVector x) {
  int prev = x[0];

  IntegerVector::iterator it;
  for (it = x.begin() + 1; it != x.end(); ++it) {
    if (prev < *it) {
      prev = *it;
    } else {
      return false;
    }
  };
  return true;
}

// If the minimal difference of  x is one
// whether needs updating interval

// [[Rcpp::export]]
bool is_min_gap_one(NumericVector x) {
  if (x.size() == 0) {
    return false;
  }
  std::sort(x.begin(), x.end());
  NumericVector y = diff(x);
  NumericVector::iterator it;

  for (it = y.begin(); it != y.end(); ++it) {
    if (*it == 1) {
      return true;
    }
  };
  return false;
}
