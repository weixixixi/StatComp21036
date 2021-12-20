#include <Rcpp.h>
using namespace Rcpp;

//' @title A Gibbs sampler using Rcpp
//' @description A Gibbs sampler using Rcpp
//' @param a parameter of beta distribution
//' @param b parameter of beta distribution
//' @param m parameter of distribution
//' @return a random sample of size \code{n}
//' @export
// [[Rcpp::export]]
NumericMatrix gib_chain_rcpp(double a, double b, double m) {
  int N = 5000;
  int burn = 1000;
  NumericMatrix X(N, 2);
  NumericVector v = {0, 0};
  X(0, _) = v;
  for (int i = 1; i < N; i++) {
    double x1 = X(i - 1, 1);
    X(i, 0) = as<int>(rbinom(1, m, x1));
    int x0 = X(i, 0);
    X(i, 1) = as<double>(rbeta(1, x0 + a, m + b - x0));
  }
  NumericMatrix x = X(Range(burn - 1, N - 1), Range(0, 1));
  return x;
}
