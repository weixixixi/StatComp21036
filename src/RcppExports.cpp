// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// gib_chain_rcpp
NumericMatrix gib_chain_rcpp(double a, double b, double m);
RcppExport SEXP _StatComp21036_gib_chain_rcpp(SEXP aSEXP, SEXP bSEXP, SEXP mSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< double >::type a(aSEXP);
    Rcpp::traits::input_parameter< double >::type b(bSEXP);
    Rcpp::traits::input_parameter< double >::type m(mSEXP);
    rcpp_result_gen = Rcpp::wrap(gib_chain_rcpp(a, b, m));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_StatComp21036_gib_chain_rcpp", (DL_FUNC) &_StatComp21036_gib_chain_rcpp, 3},
    {NULL, NULL, 0}
};

RcppExport void R_init_StatComp21036(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}