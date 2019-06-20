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
