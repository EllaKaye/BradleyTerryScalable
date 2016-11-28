#include <RcppArmadillo.h>
using namespace Rcpp;
using namespace arma;

// [[Rcpp::depends(RcppArmadillo)]]

//' @export
// [[Rcpp::export]]
List ILSR(S4 W, int maxit = 5000, double epsilon = 1e-3) {

  // Convert S4 Matrix to arma Sparse Matrix
  IntegerVector dims = W.slot("Dim");
  arma::urowvec W_i = Rcpp::as<arma::urowvec>(W.slot("i"));
  arma::urowvec W_p = Rcpp::as<arma::urowvec>(W.slot("p"));
  arma::vec W_x     = Rcpp::as<arma::vec>(W.slot("x"));

  int nrow = dims[0], ncol = dims[1];

  arma::sp_mat W_arma(W_i, W_p, W_x, nrow, ncol);

  //Rcout << "W_arma created \n";

  int K = W_arma.n_rows;

  // Kill the diagonal (a fudge, but much quicker than .diag().zeros() on sparse matrix)
  arma::mat spec(K, K, fill::ones);
  spec.diag().zeros();
  W_arma = W_arma % spec;

  // Set up N and extract what remains unchanged
  arma::sp_mat N = W_arma + W_arma.t();

  //Rcout << "N created \n";

  arma::sp_mat::const_iterator first = N.begin();
  arma::sp_mat::const_iterator last  = N.end();

  std::vector<double> nij;
  nij.reserve(N.n_nonzero);
  arma::umat N_locations(2, N.n_nonzero);

  //Rcout << "N_locations created \n";

  int ii = 0;
  for(arma::sp_mat::const_iterator it = first; it != last; ++it)
  {
    nij.push_back(*it);
    N_locations(0,ii)   = it.row();
    N_locations(1,ii++) = it.col();
  }

  // set up rowSums of W, which doesn't change during iterations
  arma::vec r = arma::vec(sum(W_arma, 1));

  //Rcout << "r created \n";

  // Extract locations of non-zero elements in W
  std::vector<double> wij;
  wij.reserve(W_arma.n_nonzero);

  arma::umat W_locations(2, W_arma.n_nonzero);
  ii = 0;
  for(arma::sp_mat::const_iterator it = W_arma.begin(); it != W_arma.end(); ++it)
  {
    wij.push_back(*it);
    W_locations(0,ii) = it.row();
    W_locations(1,ii++) = it.col();
  }

  //Rcout << "W_locations extracted \n";

  // set up pi
  arma::vec pi(K);
  pi.fill(1.0/K); // equal start

  //Rcout << "pi set up \n";

  // Create storage outside of loop
  arma::vec N_values(nij.size()); // vector of values
  arma::vec W_values(W_arma.n_nonzero);
  arma::sp_mat fitted(nrow, ncol);
  arma::vec res(K);
  arma::rowvec Wcolsum(K);
  arma::cx_vec eigvec;
  arma::cx_vec eigval;

  // set up iterations
  int iter = 0;
  bool converged = FALSE;

  //Rcout << "start iterations \n";
  // do the iterations...
  while( iter++ < maxit && !converged ) {
    //Rcout << "iter = " << iter << std::endl;

    // check for interrupt every 10 iterations
    if (iter % 10 == 0) Rcpp::checkUserInterrupt();

    // Rescale W by the latest pi and save into W
    for(int i = 0; i <  wij.size(); i++) {
      W_values[i] = wij[i] / (pi[W_locations.row(0)[i]] + pi[W_locations.row(1)[i]]);
    }
    W_arma = arma::sp_mat(W_locations, W_values, nrow, ncol);

    //Rcout << "W rescaled \n";

    // Rescale N by the latest pi and save into N
    for(int i = 0; i <  nij.size(); i++) {
      N_values[i] = nij[i] / (pi[N_locations.row(0)[i]] + pi[N_locations.row(1)[i]]);
    }
    N = arma::sp_mat(N_locations, N_values, nrow, ncol);

    //Rcout << "N rescaled \n";

    //check convergence by closeness to sufficient statistic
    arma::vec rowsums(K);
    rowsums.zeros();
    for(arma::sp_mat::const_iterator it = N.begin(); it != N.end(); ++it)
    {
      rowsums[it.row()] += *it * pi[it.row()];
    }

    //res = abs(r - rowsums);
    res = abs(r/rowsums - 1);
    converged = TRUE;

    for(int k = 0; k < res.size(); ++k) {
      if(res(k) > epsilon) {
        converged = FALSE;
        break;
      }
    }

    //Rcout << "checked convergence \n";

    //rescale by colSums so that largest eigenvalue equal to one
    //(so that the stationary distribution (pi) is equal to the largest eigenvector)
    Wcolsum = 1.0/arma::rowvec(sum(W_arma,0));

    int ii=0;
    for(arma::sp_mat::const_iterator it = W_arma.begin(); it != W_arma.end(); ++it)
    {
      W_values[ii++] = (*it) * Wcolsum[it.row()];
    }
    W_arma = arma::sp_mat(W_locations, W_values, nrow, ncol);

    //Rcout << "rescaled by colsums \n";

    //Rcout << W_arma.has_nan() << std::endl;

    // if (iter == 3) return List::create(_["W"] = W_arma);

    arma::eigs_gen(eigval, eigvec, W_arma, 1);

    // try {
    //   arma::eigs_gen(eigval, eigvec, W_arma, 1);
    // } catch(std::exception &ex) {
    //   forward_exception_to_r(ex);
    // } catch(...) {
    //   ::Rf_error("c++ exception (unknown reason)");
    // }

    //Rcout << "something weird? \n";

    pi = abs(eigvec);

    //Rcout << "eigenvector computed \n";

  } // end while loop

  //Rcout << "Loop finished \n";

  return(List::create(
      _["pi"] = pi /= sum(pi),
      _["iters"] = iter - 1,
      _["converged"] = converged));
}
