#include <Rcpp.h>
using namespace Rcpp;


// [[Rcpp::export]]
List Cintervallengths(
                 IntegerVector start_vector,
                 IntegerVector end_vector,
                 int start_scalar,
                 int end_scalar) {


  //warning this function contains no error checks
  // ie no check for negative intervals or vectors of different lengths
   //all of this is assumed to have been done in R
    //ie in the intervalaverage function


  int n = start_vector.length();

  if(start_vector[0] == NA_INTEGER){
    //if interval is missing, this means there wasn't a join
    // value=NA, xduration=0, nobs_value=0, xminstart=NA, xmaxend=NA
    List L = List::create(Named("xduration") = 0,
                          Named("xminstart") = NA_INTEGER,
                          Named("xmaxend") = NA_INTEGER,
                          Named("durations") = rep(0,n));
    return L;

  }



  // initialize intersect interval according to the start and end scalar
   //ie the interval from "y" in the R function

  IntegerVector interval_intersect_start(n, start_scalar);
  IntegerVector interval_intersect_end(n, end_scalar);
  IntegerVector intersectlength(n);

  int sum_intersectlength = 0;



  //take the max of the starts
  if(start_vector[0] > interval_intersect_start[0]){
    interval_intersect_start[0] = start_vector[0];
  }

  //take the min of the ends
  if(end_vector[0] < interval_intersect_end[0]){
    interval_intersect_end[0] = end_vector[0];
  }

  intersectlength[0] = interval_intersect_end[0]-interval_intersect_start[0] + 1;
  sum_intersectlength = sum_intersectlength + intersectlength[0];

  int xminstart = interval_intersect_start[0];
  int xmaxend = interval_intersect_end[0];


  for(int i = 1; i < n; i++){

    //take the max of the starts
    if(start_vector[i] > interval_intersect_start[i]){
      interval_intersect_start[i] = start_vector[i];
    }

    //take the min of the ends
    if(end_vector[i] < interval_intersect_end[i]){
      interval_intersect_end[i] = end_vector[i];
    }

    intersectlength[i] = interval_intersect_end[i]-interval_intersect_start[i] + 1;
    sum_intersectlength = sum_intersectlength + intersectlength[i];

    if(interval_intersect_start[i] < xminstart){
      xminstart = interval_intersect_start[i];
    }

    if(interval_intersect_end[i] > xmaxend){
      xmaxend = interval_intersect_end[i];
    }

  }




  List L = List::create(Named("xduration") = sum_intersectlength,
                        Named("xminstart") = xminstart,
                        Named("xmaxend") = xmaxend,
                        Named("durations") = intersectlength
                        );
  return L;
}




// [[Rcpp::export]]
List Cweighted_mean(
    NumericVector values,
    IntegerVector durations
    ){

  int n = values.length();
  double sum_product = 0;
  int sum_durations = 0;

  //always remove NAs
  for(int i = 0; i < n; i++){
    if(!NumericVector::is_na(values[i])){
      if(durations[i] > 0){
        sum_product = sum_product + durations[i]*values[i];
        sum_durations = sum_durations + durations[i];
      }
    }
  // note that sum_duration is a value-specific version of xduration
    // that only sums the duration where the value is nonmissing

  }

  List L = List::create(Named("average") = sum_product/sum_durations,
                        Named("nobs_var") = sum_durations
                        );
  return L;
}

