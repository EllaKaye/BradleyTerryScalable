#include <RcppArmadillo.h>
using namespace Rcpp;
using namespace arma;

// [[Rcpp::depends(RcppArmadillo)]]

// [[Rcpp::export]]
List BT_EM(S4 W, double a, double b, int maxit = 5000, double epsilon = 1e-3) {

  // Convert S4 Matrix to arma Sparse Matrix
  IntegerVector dims = W.slot("Dim");
  arma::urowvec w_i = Rcpp::as<arma::urowvec>(W.slot("i"));
  arma::urowvec w_p = Rcpp::as<arma::urowvec>(W.slot("p"));
  arma::vec w_x     = Rcpp::as<arma::vec>(W.slot("x"));

  int nrow = dims[0], ncol = dims[1];

  arma::sp_mat W_arma(w_i, w_p, w_x, nrow, ncol);

  int K = W_arma.n_rows;

  // Set diagonal of W to zero
  // Fudge: element-wise multiplication of W by ones matrix with zeros on diagonal
  // NB much faster than W.diag().zeros()
  arma::mat spec(K, K, fill::ones);
  spec.diag().zeros();
  W_arma = W_arma % spec;

  // Set up N and store original values
  arma::sp_mat N = W_arma + W_arma.t();
  arma::sp_mat N_orig = N;

  arma::sp_mat::const_iterator first = N.begin();
  arma::sp_mat::const_iterator last   = N.end();

  std::vector<double> nij;
  nij.reserve(N.n_nonzero);
  arma::umat n_locations(2, N.n_nonzero);

  int ii=0;
  for(arma::sp_mat::const_iterator it = first; it != last; ++it)
  {
    nij.push_back(*it);
    n_locations(0,ii)   = it.row();
    n_locations(1,ii++) = it.col();
  }

  // set up numerator
  arma::vec numer = arma::vec(sum(W_arma, 1)) + (a - 1);

  // set up pi
  arma::vec pi(K);
  pi.fill(1.0/K); // equal start

  bool use_eigs = !any(arma::rowvec(sum(W_arma,0)) == 0);

  if(use_eigs && (K > 2)) {
    arma::cx_vec eigvec;
    arma::cx_vec eigval;

    // update W so its divided by colSums
    arma::rowvec Wcolsum = 1.0/arma::rowvec(sum(W_arma,0));
    arma::umat w_locations(2, W_arma.n_nonzero);
    arma::vec values(W_arma.n_nonzero);

    int ii=0;
    for(arma::sp_mat::const_iterator it = W_arma.begin(); it != W_arma.end(); ++it)
    {
      values[ii] = (*it) * Wcolsum[it.row()];
      w_locations(0,ii) = it.row();
      w_locations(1,ii++) = it.col();
    }
    W_arma = arma::sp_mat(w_locations, values, W_arma.n_rows, W_arma.n_cols);

    arma::eigs_gen(eigval, eigvec, W_arma, 1);
    pi = abs(eigvec);
  } // end eigenvector for pi

  // Create storage outside of while loop
  arma::vec values(nij.size()); // vector of values
  arma::vec res(K);
  arma::vec denom(K);
  arma::vec rowsums(K);

  // set up iterations
  int iter = 0;
  bool converged = FALSE;

  while( iter++ < maxit && !converged ) {

    // check for interrupt every 10 iterations
    if (iter % 10 == 0) Rcpp::checkUserInterrupt();

    // E step
    //// update 'values' and batch insert back into N
    for(int i = 0; i <  nij.size(); i++) {
      values[i] = nij[i] / (pi[n_locations.row(0)[i]] + pi[n_locations.row(1)[i]]);
    }
    N = arma::sp_mat(n_locations, values, nrow, ncol);

    // check convergence
    rowsums.zeros();
    for(arma::sp_mat::const_iterator it = N.begin(); it != N.end(); ++it)
    {
      rowsums[it.row()] += *it * pi[it.row()];
    }

    rowsums += b * pi;
    //res = abs(numer - rowsums);
    res = abs(numer/rowsums - 1);
    converged = TRUE;

    for(int k = 0; k < res.size(); ++k) {
      if(res(k) > epsilon) {
        converged = FALSE;
        break;
      }
    }

    // M step
    denom = arma::vec(sum(N, 1)) + b;
    pi = numer / denom;
  } // end while loop

  return(List::create(
      _["pi"] = pi /= sum(pi),
      _["iters"] = iter - 1,
      _["converged"] = converged,
      _["N"] = N_orig));
}
