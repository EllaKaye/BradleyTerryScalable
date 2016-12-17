#include <RcppArmadillo.h>
using namespace Rcpp;
using namespace arma;

// [[Rcpp::depends(RcppArmadillo)]]

//' @export
// [[Rcpp::export]]
arma::sp_mat btprob_vec_sp(arma::vec pi) {

  arma::mat outer(pi.size(), pi.size());
  outer.each_col() = pi;
  outer.each_row() += pi.t();
  outer = 1/outer;
  outer.diag().zeros();
  outer.each_col() %= pi;

  arma::sp_mat outer_sp(outer);

  return outer_sp;
}

//' @export
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

//' @export
// [[Rcpp::export]]
arma::mat btfitted_vec_d(arma::vec pi, arma::mat N) {

  arma::mat outer(pi.size(), pi.size());
  outer.each_col() = pi;
  outer.each_row() += pi.t();
  outer = 1/outer;
  outer.diag().zeros();
  outer.each_col() %= pi;

  outer %= N;

  return outer;
}

//' @export
// [[Rcpp::export]]
arma::mat btfitted_vec_sp(arma::vec pi, arma::sp_mat N) {

  arma::mat outer(pi.size(), pi.size());
  outer.each_col() = pi;
  outer.each_row() += pi.t();
  outer = 1/outer;
  outer.diag().zeros();
  outer.each_col() %= pi;

  outer %= N;

  return outer;
}

//' @export
// [[Rcpp::export]]
arma::sp_mat btfitted_vec_sp_sp(arma::vec pi, arma::sp_mat N) {

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

