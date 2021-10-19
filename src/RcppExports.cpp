// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// count_precedence
DataFrame count_precedence(CharacterVector cases, IntegerVector activities, int lead);
RcppExport SEXP _processmapR_count_precedence(SEXP casesSEXP, SEXP activitiesSEXP, SEXP leadSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< CharacterVector >::type cases(casesSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type activities(activitiesSEXP);
    Rcpp::traits::input_parameter< int >::type lead(leadSEXP);
    rcpp_result_gen = Rcpp::wrap(count_precedence(cases, activities, lead));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_processmapR_count_precedence", (DL_FUNC) &_processmapR_count_precedence, 3},
    {NULL, NULL, 0}
};

RcppExport void R_init_processmapR(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
