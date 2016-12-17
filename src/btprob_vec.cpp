#include <RcppArmadillo.h>
using namespace Rcpp;
using namespace arma;

// [[Rcpp::depends(RcppArmadillo)]]

// [[Rcpp::export]]
arma::mat btprob_vec(arma::vec pi) {

  arma::mat outer(pi.size(), pi.size());
  outer.each_col() = pi;
  outer.each_row() += pi.t();
  outer = 1/outer;
  outer.diag().zeros();
  outer.each_col() %= pi;

  return outer;
}

// [[Rcpp::export]]
arma::sp_mat btfitted_vec(arma::vec pi, arma::sp_mat N) {

  arma::mat outer(pi.size(), pi.size());
  outer.each_col() = pi;
  outer.each_row() += pi.t();
  outer = 1/outer;
  outer.diag().zeros();
  outer.each_col() %= pi;

  outer %= N;

  arma::sp_mat outer_sp(outer);

  return outer_sp;
}

