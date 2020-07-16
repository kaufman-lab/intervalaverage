#include <Rcpp.h>
#include "concatenate_lists.h"
using namespace Rcpp;


// [[Rcpp::export]]
List Cintervalaverage(
                 NumericMatrix values_matrix,
                 IntegerVector start_vector,
                 IntegerVector end_vector,
                 int start_scalar,
                 int end_scalar) {


  //warning this function contains no error checks
  // ie no check for negative intervals or vectors of different lengths
   //all of this is assumed to have been done in R
    //ie in the intervalaverage function

  int n_values = values_matrix.ncol();
  int n = start_vector.length();

  List values_list = List::create(2*n_values);


  if(start_vector[0] == NA_INTEGER){
    //if interval is missing, this means there wasn't a join
    // value=NA, xduration=0, nobs_value=0, xminstart=NA, xmaxend=NA
    List L = List::create(Named("xduration") = 0,
                          Named("xminstart") = NA_INTEGER,
                          Named("xmaxend") = NA_INTEGER);

    for(int j = 0; j < n_values; j++){

      values_list[2*j] = NA_REAL;
      values_list[2*j + 1] = 0;

    }

    return concatenate_lists(L,values_list);

  }



  // initialize intersect interval according to the start and end scalar
   //ie the interval from "y" in the R function

  IntegerVector interval_intersect_start(n, start_scalar);
  IntegerVector interval_intersect_end(n, end_scalar);
  IntegerVector intersectlength(n);

  int sum_intersectlength = 0;



  //initialzing xminstart depends
   //on knowing the intersect start and intersect end
   //calculate the first intervel intersects manually outside of the loop
  int xminstart = start_scalar;
  if(start_vector[0] > start_scalar){
    xminstart = start_vector[0];
  }
  int xmaxend = end_scalar;
  if(end_vector[0] < end_scalar){
    xmaxend = end_vector[0];
  }


  NumericVector sum_product(n_values, 0.0);
  IntegerVector sum_durations(n_values, 0.0);


  for(int j = 0; j < n_values; j++){

    NumericVector values = values_matrix( _ , j );

    for(int i = 0; i < n; i++){

      if(i ==0){
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
      } //end if i==0

      if(!NumericVector::is_na(values[i] )){
        if(intersectlength[i] > 0){
          //all elements of values_list assumed to be numeric
          sum_product[j] = sum_product[j] + intersectlength[i]*values[i];
          sum_durations[j] = sum_durations[j] + intersectlength[i];
        }
      }


    }

    values_list[2*j] = sum_product[j]/sum_durations[j];
    values_list[2*j + 1] = sum_product[j]/sum_durations[j];


  }


    List L = List::create(Named("xduration") = sum_intersectlength,
                          Named("xminstart") = xminstart,
                          Named("xmaxend") = xmaxend
    );


  return concatenate_lists(L,values_list);
}







