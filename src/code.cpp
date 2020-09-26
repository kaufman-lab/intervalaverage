#include <Rcpp.h>
#include "concatenate_lists.h"
using namespace Rcpp;


// [[Rcpp::export]]
List Cintervalaverage(
                 List values_list,
                 IntegerVector start_vector,
                 IntegerVector end_vector,
                 int start_scalar,
                 int end_scalar,
                 CharacterVector value_names
                 ) {


  //warning this function contains no error checks
  // ie no check for negative intervals or vectors of different lengths
   //all of this is assumed to have been done in R
    //ie in the intervalaverage function

  int n_values = values_list.size();
  int n = start_vector.length();


  List avg_value_and_nobs_and_maxgaps_list(3*n_values);


  avg_value_and_nobs_and_maxgaps_list.names() = value_names;


  if(start_vector[0] == NA_INTEGER){
    //if interval is missing, this means there wasn't a join
    // value=NA, xduration=0, nobs_value=0, xminstart=NA, xmaxend=NA,
     // maxgap_value=length of y interval
    List L = List::create(Named("xduration") = 0,
                          Named("xminstart") = NA_INTEGER,
                          Named("xmaxend") = NA_INTEGER);

    for(int j = 0; j < n_values; j++){

      avg_value_and_nobs_and_maxgaps_list[3*j] = NA_REAL;
      avg_value_and_nobs_and_maxgaps_list[3*j + 1] = 0;
      avg_value_and_nobs_and_maxgaps_list[3*j + 2] = end_scalar - start_scalar + 1;

    }

    return concatenate_lists(L,avg_value_and_nobs_and_maxgaps_list);

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
  IntegerVector sum_durations(n_values, 0);
  IntegerVector maxgap(n_values, 0);

  // last_observed_time is going to be used to keep track of structural missingness
  // ie if there are gaps between the intervals defined by the
     // combination of start_vector and end_vector, the length of these gaps
      //will be added to the running count of gaps
      //note that the length of any structural gap is:
         //last_observed_time time - current time - 1
        // this is because all defined intervals are inclusive
         // so to take the length of the gap it's really
           //(last_observed_time - 1) - (curent_time + 1) + 1
  int last_observed_time = start_scalar - 1;
  int current_gap_count = 0;

  //j indexes over value columns
  //i indexes over rows
  for(int j = 0; j < n_values; j++){

    NumericVector values = values_list[j];
    last_observed_time = start_scalar - 1;
    current_gap_count = 0;

    for(int i = 0; i < n; i++){

      if(j ==0 ){

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
      } //end if j==0

      current_gap_count = current_gap_count + interval_intersect_start[i] - last_observed_time - 1;
      last_observed_time = interval_intersect_end[i];

      if(!NumericVector::is_na(values[i] )){
        if(intersectlength[i] > 0){
          //all elements of avg_value_and_nobs_and_maxgaps_list assumed to be numeric
          sum_product[j] = sum_product[j] + intersectlength[i]*values[i];
          sum_durations[j] = sum_durations[j] + intersectlength[i];

          if(current_gap_count > maxgap[j]){
            maxgap[j] = current_gap_count;
          }
          current_gap_count = 0;

        }
      } else {
        // missing values are ignored in calculating the average,
         // but we still need to pay attention to them for calculating max gaps:
         // note that we're assuming that the intervals defined in
          //star_vector/end_vector are ordered and non-overlapping
        current_gap_count = current_gap_count + intersectlength[i];

        if(current_gap_count > maxgap[j]){
          maxgap[j] = current_gap_count;
        }

      } // end if non-missing values[i]


    } // end i loop

    //check for gap between the last structurally observed interval and the end of the averaging period:
    current_gap_count = current_gap_count + end_scalar - last_observed_time;
     //don't subtract one here since the gap need to be
       /// inclusive of end_scalar but exclusive of last_observed_time
    if(current_gap_count > maxgap[j]){
      maxgap[j] = current_gap_count;
    }

    avg_value_and_nobs_and_maxgaps_list[3*j] = sum_product[j]/sum_durations[j];
    avg_value_and_nobs_and_maxgaps_list[3*j + 1] = sum_durations[j];
    avg_value_and_nobs_and_maxgaps_list[3*j + 2] =  maxgap[j];


  } // end j loop



    List L = List::create(Named("xduration") = sum_intersectlength,
                          Named("xminstart") = xminstart,
                          Named("xmaxend") = xmaxend
    );


  return concatenate_lists(L,avg_value_and_nobs_and_maxgaps_list);
}







