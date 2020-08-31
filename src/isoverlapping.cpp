#include <Rcpp.h>
using namespace Rcpp;


// [[Rcpp::export]]
LogicalVector Cisoverlapping(
          IntegerVector start_vector,
          IntegerVector end_vector
          ) {
  // start_vector is assumed to be sorted
  // end_vector is assumed to be sorted within values of start_vector

  bool out = NA_LOGICAL;
  int n = start_vector.size();

  for(int i = 1; i < n; i++){
    if(start_vector[i] <= end_vector[i-1]){
      out = TRUE;
      return out;
    }
  }

  out = FALSE;
  return out;
}
