// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// Cisoverlapping
LogicalVector Cisoverlapping(IntegerVector start_vector, IntegerVector end_vector);
RcppExport SEXP _intervalaverage_Cisoverlapping(SEXP start_vectorSEXP, SEXP end_vectorSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< IntegerVector >::type start_vector(start_vectorSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type end_vector(end_vectorSEXP);
    rcpp_result_gen = Rcpp::wrap(Cisoverlapping(start_vector, end_vector));
    return rcpp_result_gen;
END_RCPP
}
// Cintervalaverage
List Cintervalaverage(List values_list, IntegerVector start_vector, IntegerVector end_vector, int start_scalar, int end_scalar, CharacterVector value_names);
RcppExport SEXP _intervalaverage_Cintervalaverage(SEXP values_listSEXP, SEXP start_vectorSEXP, SEXP end_vectorSEXP, SEXP start_scalarSEXP, SEXP end_scalarSEXP, SEXP value_namesSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< List >::type values_list(values_listSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type start_vector(start_vectorSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type end_vector(end_vectorSEXP);
    Rcpp::traits::input_parameter< int >::type start_scalar(start_scalarSEXP);
    Rcpp::traits::input_parameter< int >::type end_scalar(end_scalarSEXP);
    Rcpp::traits::input_parameter< CharacterVector >::type value_names(value_namesSEXP);
    rcpp_result_gen = Rcpp::wrap(Cintervalaverage(values_list, start_vector, end_vector, start_scalar, end_scalar, value_names));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_intervalaverage_Cisoverlapping", (DL_FUNC) &_intervalaverage_Cisoverlapping, 2},
    {"_intervalaverage_Cintervalaverage", (DL_FUNC) &_intervalaverage_Cintervalaverage, 6},
    {NULL, NULL, 0}
};

RcppExport void R_init_intervalaverage(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
