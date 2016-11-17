#include <RcppArmadillo.h>
using namespace Rcpp;
using namespace arma;

// [[Rcpp::depends(RcppArmadillo)]]

// [[Rcpp::export]]
List ILSR_arma(S4 W_R, int maxit = 100, double epsilon = 1e-2) {
  //Rcout << "Start C++\n";

  // Convert S4 Matrix to arma Sparse Matrix
  //Rcout << "Converting W_R to W\n";
  IntegerVector dims = W_R.slot("Dim");
  arma::urowvec W_i = Rcpp::as<arma::urowvec>(W_R.slot("i"));
  arma::urowvec W_p = Rcpp::as<arma::urowvec>(W_R.slot("p"));
  arma::vec W_x     = Rcpp::as<arma::vec>(W_R.slot("x"));

  int nrow = dims[0], ncol = dims[1];

  arma::sp_mat W(W_i, W_p, W_x, nrow, ncol);

  int K = W.n_rows;

  // Kill the diagonal (a fudge, but much quicker than .diag().zeros() on sparse matrix)
  arma::mat spec(K, K, fill::ones);
  spec.diag().zeros();
  W = W % spec;
  //Rcpp::Rcout << "diag equal to zero\n";

  arma::sp_mat N = W + W.t();
  //Rcpp::Rcout << "N is set up\n";

  // set up rowSums of W, which doesn't change during iterations
  arma::vec r = arma::vec(sum(W, 1));

  // Extract locations of non-zero elements in W

  std::vector<double> wij;
  wij.reserve(W.n_nonzero);

  arma::umat W_locations(2, W.n_nonzero);
  int ii = 0;
  for(arma::sp_mat::const_iterator it = W.begin(); it != W.end(); ++it)
  {
    wij.push_back(*it);
    W_locations(0,ii) = it.row();
    W_locations(1,ii++) = it.col();
  }

  //// Set up N and extract what remains unchanges
  //Rcpp::Rcout << "Setting up N\n";

  // Store original values from N
  //Rcout << "Begin storing values from N\n";
  arma::sp_mat::const_iterator first = N.begin();
  arma::sp_mat::const_iterator last  = N.end();

  std::vector<double> nij;
  nij.reserve(N.n_nonzero);
  arma::umat N_locations(2, N.n_nonzero);

  ii = 0;
  for(arma::sp_mat::const_iterator it = first; it != last; ++it)
  {
    nij.push_back(*it);
    N_locations(0,ii)   = it.row();
    N_locations(1,ii++) = it.col();

  }

  //Rcout << "End storing values from N into nij and n_locations\n";

  // set up pi (at a later stage we'll deal with situation where we modify W to get the starting values)
  //Rcpp::Rcout << "Setting up pi\n";
  arma::vec pi(K);
  pi.fill(1.0/K); // equal start

  // Create storage outside of loop
  arma::vec N_values(nij.size()); // vector of values
  arma::vec W_values(W.n_nonzero);
  arma::sp_mat fitted(nrow, ncol);
  arma::vec res(K);
  arma::rowvec Wcolsum(K);
  arma::cx_vec eigvec;
  arma::cx_vec eigval;


  // set up iterations
  int iter = 0;
  bool converged = FALSE;

  //Rcout << "Starting iterations ....\n";
  while( iter++ < maxit && !converged ) {
    //Rcout << "it=" << iter << "\n";

    // Rescale W by the latest pi and save into W
    //Rcout << "Rescale W\n";
    for(int i = 0; i <  wij.size(); i++) {
      W_values[i] = wij[i] / (pi[W_locations.row(0)[i]] + pi[W_locations.row(1)[i]]);
    }
    W = arma::sp_mat(W_locations, W_values, nrow, ncol);

    // Rescale N by the latest pi and save into N
    //Rcout << "Rescale N\n";
    for(int i = 0; i <  nij.size(); i++) {
      N_values[i] = nij[i] / (pi[N_locations.row(0)[i]] + pi[N_locations.row(1)[i]]);
    }
    N = arma::sp_mat(N_locations, N_values, nrow, ncol);

    //check convergence by closeness to sufficient statistic
    //Rcout << "Check convergence\n";
    arma::vec rowsums(K);
    rowsums.zeros();
    for(arma::sp_mat::const_iterator it = N.begin(); it != N.end(); ++it)
    {
      rowsums[it.row()] += *it * pi[it.row()];
    }

    res = abs(r - rowsums);
    converged = TRUE;

    for(int k = 0; k < res.size(); ++k) {
      if(res(k) > epsilon) {
        converged = FALSE;
        break;
      }
    }

    //rescale by colSums so that largest eigenvalue equal to one
    //(so that the stationary distribution (pi) is equal to the largest eigenvector)

    //Rcout << "Rescale by colSums\n";
    Wcolsum = 1.0/arma::rowvec(sum(W,0));

    int ii=0;
    for(arma::sp_mat::const_iterator it = W.begin(); it != W.end(); ++it)
    {
      W_values[ii++] = (*it) * Wcolsum[it.row()];
    }
    W = arma::sp_mat(W_locations, W_values, W.n_rows, W.n_cols);

    //Wcolsum.print("colSums W");

    // find dominant eigenvector
    //Rcout << "Find eigenvector\n";
    arma::eigs_gen(eigval, eigvec, W, 1);
    //Rcout << "Found eigenvector\n";
    pi = abs(eigvec);
    //Rcout << "End iteration\n";

  } // end while loop

  return(List::create(
      _["pi"] = pi /= sum(pi),
      _["iters"] = iter - 1,
      _["converged"] = converged));
}
