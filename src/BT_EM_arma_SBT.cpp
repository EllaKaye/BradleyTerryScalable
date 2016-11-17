#include <RcppArmadillo.h>
using namespace Rcpp;
using namespace arma;

// [[Rcpp::depends(RcppArmadillo)]]

// [[Rcpp::plugins(cpp11)]]

// [[Rcpp::export]]
List BT_EM_arma(S4 W_R, double a, double b, int maxit = 100, double epsilon = 1e-2) {

    //Rcout << "Start C++\n";

  // Convert S4 Matrix to arma Sparse Matrix
  //Rcout << "Converting W_R to W\n";
  IntegerVector dims = W_R.slot("Dim");
  arma::urowvec w_i = Rcpp::as<arma::urowvec>(W_R.slot("i"));
  arma::urowvec w_p = Rcpp::as<arma::urowvec>(W_R.slot("p"));
  arma::vec w_x     = Rcpp::as<arma::vec>(W_R.slot("x"));

  int nrow = dims[0], ncol = dims[1];

  arma::sp_mat W(w_i, w_p, w_x, nrow, ncol);

  int K = W.n_rows;

  // Set up N and extract what remains unchanges
  //Rcpp::Rcout << "Setting up N\n";
  // NOTE DIAG SET TO ZERO COPIED OUT!!! TRY BATCH INSERT OF ZEROS WITH LINSPACED DIAGONAL LOCATIONS
  //W.diag().zeros();
  //arma::umat w_locations(2, W.n_nonzero);
  //FILL THIS IN LINSPACED
  //w_locations.row(0) = arma::linspace<urowvec>(0, K-1, K);
  //w_locations.row(1) = arma::linspace<urowvec>(0, K-1, K);

  //arma::vec diag(K,fill::zeros);


  // Fudge: try element-wise multiplication of special matrix with W
  // NB much faster than W.diag().zeros()
  arma::mat spec(K, K, fill::ones);
  spec.diag().zeros();
  W = W % spec;
  //Rcpp::Rcout << "diag equal to zero\n";

  //W.diag().zeros();
  arma::sp_mat N = W + W.t();
  //Rcout << "K is " << K << std::endl;
  //Rcpp::Rcout << "N is set up\n";

  // set up numerator
  //Rcpp::Rcout << "Setting up numerator\n";
  arma::vec numer = arma::vec(sum(W, 1)) + (a - 1);
  //arma::vec avec(K);
  //avec.fill(a-1.0);
  //numer += avec;
  //Rcpp::Rcout << "Numerator is set up\n";

  // set up pi (at a later stage we'll deal with situation where we modify W to get the starting values)
  //Rcpp::Rcout << "Setting up pi\n";
  arma::vec pi(K);
  pi.fill(1.0/K); // equal start

  bool use_eigs = !any(arma::rowvec(sum(W,0)) == 0);

  //Rcout << "use_eigs is " << use_eigs << std::endl;

  if(use_eigs && (K > 2)) {
    arma::cx_vec eigvec;
    arma::cx_vec eigval;

    // update W so its divided by colSums
    // OK to modify W in this way - we don't use it again
    // arma::sp_mat Wcolsum = sum(W,0);
    // for(int i=0; i<K; i++) {
    //   W.row(i) /= Wcolsum[i];
    // }
    arma::rowvec Wcolsum = 1.0/arma::rowvec(sum(W,0));
    arma::umat w_locations(2, W.n_nonzero);
    arma::vec values(W.n_nonzero);

    // CONSTRUCT VALUES VECTOR AND BATCH INSERT
    int ii=0;
    for(arma::sp_mat::const_iterator it = W.begin(); it != W.end(); ++it)
    {
      values[ii] = (*it) * Wcolsum[it.row()];
      w_locations(0,ii) = it.row();
      w_locations(1,ii++) = it.col();
    }
    W = arma::sp_mat(w_locations, values, W.n_rows, W.n_cols);

    //Rcout << "Begin eigendecomposition\n";
    arma::eigs_gen(eigval, eigvec, W, 1);
    pi = abs(eigvec);
    //Rcout << "End eigendecomposition\n";
  } // end eigenvector for pi

  //pi.print("pi, possibly after eigs");

  // Store original values from N
  //Rcout << "Begin storing values from N\n";
  arma::sp_mat::const_iterator first = N.begin();
  arma::sp_mat::const_iterator last   = N.end();

  std::vector<double> nij;
  // std::vector<int> nijRows, nijCols;
  nij.reserve(N.n_nonzero);
  // nijRows.reserve(N.n_nonzero);
  // nijCols.reserve(N.n_nonzero);
  arma::umat n_locations(2, N.n_nonzero);

  int ii=0;
  for(arma::sp_mat::const_iterator it = first; it != last; ++it)
  {
    //Rcpp::Rcout << "location: " << it.row() << "," << it.col() << "  ";
    //Rcpp::Rcout << "value: " << (*it) << std::endl;

    nij.push_back(*it);
    n_locations(0,ii)   = it.row();
    n_locations(1,ii++) = it.col();
    // nijRows.push_back(it.row());
    // nijCols.push_back(it.col());
  }
  //Rcout << "End storing values from N into std\n";

  //// Convert to arma::vec and save in locations matrix
  // arma::uvec nijRows_arma = conv_to<uvec>::from(nijRows);
  // arma::uvec nijCols_arma = conv_to<uvec>::from(nijCols);

  // arma::umat n_locations(2, nij.size());
  // n_locations.row(0) = nijRows_arma.t();
  // n_locations.row(1) = nijCols_arma.t();
  //Rcout << "End storing values from std::vector into arma::vec\n";



  // Create storage outside of loop
  arma::vec values(nij.size()); // vector of values
  //arma::sp_mat fitted(nrow, ncol);
  arma::vec res(K); // if res not sparse, make dense
  arma::vec denom(K);
  //arma::vec bvec(K);
  //bvec.fill(b);
  arma::vec rowsums(K);

  //arma::vec pi_new(K);// do I need this?

  // set up iterations
  int iter = 0;
  bool converged = FALSE;

  //Rcout << "Starting iterations ....\n";
  while( iter++ < maxit && !converged ) {
    //Rcout << "it=" << iter << "\n";
    //pi.print();
    // E step
    //// update 'values' (N@x in R code equivalent)
    for(int i = 0; i <  nij.size(); i++) {
      // values[i] = nij[i] / (pi[nijRows_arma[i]] + pi[nijCols_arma[i]]);
      values[i] = nij[i] / (pi[n_locations.row(0)[i]] + pi[n_locations.row(1)[i]]);
    }
    //// put values back in sparse matrix N with batch insert
    //N = std::move(arma::sp_mat(n_locations, values, nrow, ncol));
    N = arma::sp_mat(n_locations, values, nrow, ncol);
    //N.print();

    // check convergence here in non-penalised case
    //if(a == 1.0 && b == 0.0) {

      // update fitted matrix
      //fitted = N;
      //for(int i=0; i<K; i++) {
      //  fitted.row(i) *= pi[i];
      //}

      rowsums.zeros();
      for(arma::sp_mat::const_iterator it = N.begin(); it != N.end(); ++it)
      {
        rowsums[it.row()] += *it * pi[it.row()];
      }

      rowsums += b * pi;

      // res = abs(numer - sum(fitted, 1));
      res = abs(numer - rowsums);
      converged = TRUE;

      //for(arma::sp_vec::const_iterator it = res.begin(); it != res.end(); ++it) {
      //  if(*it > epsilon) {
      //    converged = FALSE;
      //    break;
      //  }
      // }

      for(int k = 0; k < res.size(); ++k) {
        if(res(k) > epsilon) {
          converged = FALSE;
          break;
        }
      }
    //} // end check convergence

    // M step
    denom = arma::vec(sum(N, 1)) + b;
    //denom += bvec;

    //Rcpp::Rcout << "numer: " << numer << std::endl;
    //Rcpp::Rcout << "denom: " << denom << std::endl;

    //for (int i = 0; i < numer.size(); i++) {
    //  pi_new(i) = numer(i) / denom(i);
    //}

    pi = numer / denom;

    //Rcpp::Rcout << "pi_new (norm): " << pi_new / sum(pi_new) << std::endl;

    // check convergence here in penalised case
    // if(!(a == 1.0 && b == 0.0)) {
    //   Rcout << "Shouldn't be here\n";
    //   arma::vec change(K);
    //   change = pi_new/sum(pi_new) - pi/sum(pi);
    //   double temp = sqrt(sum(change));
    //   converged = temp < epsilon;
    // }

    // fudge to assign pi_new to pi
    // OR CAN WE WORK WITH DENSE VECTORS THROUGHOUT?
    //arma::vec pi_new2(pi_new);
    //pi = pi_new;
  }


  return(List::create(
      _["pi"] = pi /= sum(pi),
      _["iters"] = iter - 1,
      _["converged"] = converged));
}
