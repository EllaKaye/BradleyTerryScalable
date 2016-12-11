#' Fits the Bradley-Terry model
#'
#' \code{btfit} fits the Bradley-Terry model on (potentially) large and sparse datasets.
#'
#' Let there be \eqn{K} players, let \eqn{\pi_k} be the Bradley-Terry strength parameter of player \eqn{k}, for \eqn{k = 1, \ldots, K} and let \eqn{\pi} be the vector of all the \eqn{\pi_k}. Let \eqn{w_{ij}} be the number of times player \eqn{i} wins against player \eqn{j} and let \eqn{n_{ij} = w_{ij} + w_{ji}} be the number of times they play, with \eqn{w_{ii} = 0} by convention.
#'
#' Explain G_W.
#'
#' @param a Must be >= 1. When \code{a = 1}, the function returns the MLE estimate of \eqn{pi} (by component, if necessary). When \code{a > 1}, a is the shape parameter for the Gamma prior (See Details).
#' @param b The rate parameter for the Gamma prior (See Details). When \code{a = 1}, \code{btfit} returns the MLE and this argument is ignored. If \code{b = NULL} (the default) when \code{a > 1}, then \code{b} is set to \eqn{aK - 1}.
#' @param components The fully-connected components of \eqn{G_W}, as calculated by \code{\link{connected_components}}. If the MLE is requested (i.e. if \code{a = 1}) and \code{components = NULL}, \code{btfit} will calculate the components within the function, but for large matrices, this is time consuming, so we recommend passing in the components as an argument (especially as \code{\link{connected_components(W)}} should be run before \code{btfit} in any case). If \code{a > 1} then components are not necessary, but if they are provided, the function will return the penalised \eqn{pi} by component.
#' @param ML_method The algorithm to be used when finding the MLE. See Details.
#' @param maxit The maximum number of iterations for the algorithm. If returning \code{pi} by component, this will be the maximum number of iterations for each component.
#' @param epsilon Determines when the algorithm is deemed to have converged. See Details.
#' @inheritParams connected_components
#'@return Returns a list containing:
#'\item{pi}{The stregth parameter for the Bradley-Terry model. If \code{a = 1}, this is the MLE. If \code{a > 1}, this is the MAP estimate. If components are provided, or the MLE estimate is requested but \eqn{G_W} is not fully connected, then \code{pi} is list of length \eqn{N}, where \eqn{N} is the number of fully connected components of \eqn{G_W} of two or more players. The \eqn{n}-th item is a vector \eqn{pi} for the players in the \eqn{n}-th fully connected component, \eqn{n = 1, \ldots, N}.}
#'\item{iters}{A vector of length \eqn{N}. The \eqn{n}-th entry is the number of iterations it took for algorithm for component \eqn{n} to converge, for \eqn{n = 1, \ldots, N}. Note that if the algorithm has not converged in any component, a warning will be produced.}
#'\item{converged}{A logical vector of length \eqn{N}, indicating whether the algorithm has converged for the \eqn{n}-th component in \code{maxit} iterations}
#'
#'
#' @seealso \code{\link{connected_components}}, \code{\link{btprobs}}
#'
#'
#'
#' @references Caron, F. and Doucet, A. (2012) Efficient Bayesian Inference for Generalized Bradley-Terry Models. \emph{Journal of Computational and Graphical Statistics}, \strong{21}(1), 174-196.
#' @references Hunter, D. R. (2004) MM Algorithms for Generalized Bradley-Terry Models. \emph{The Annals of Statistics}, \strong{32}(1), 384-406.
#' @examples
#' W_connected <- Matrix::rsparsematrix(10, 10 , 0.5, rand.x = function(n) rbinom(n, 10, 0.5))
#' i <- c(3,1,5,4,2,5,5,7,8,5,6,8,7)
#' j <- c(1,2,2,3,4,4,6,6,6,7,7,7,8)
#' W_not_connected <-  Matrix::sparseMatrix(i = i, j = j, x = 1:13, dims = c(8,8), dimnames = list(letters[1:8], letters[1:8]))
#' W_components <- connected_components(W_not_connected)$components
#' fit1 <- btfit(W_connected, 1)
#' fit2 <- btfit(W_not_connected, 1, components = W_components)
#' fit3 <- btfit(W_not_connected, 3)
#' @export

btfit <- function(W, a, b = NULL, components = NULL, ML_method = c("MM", "ILSR"), maxit = 10000, epsilon = 1e-3) {

  call <- match.call()

  ### Checks on the arguments
  if (!(is(W, "Matrix") | is.matrix(W) )) stop("W must be a square matrix")
  if (dim(W)[1] != dim(W)[2]) stop("W must be a square matrix")
  if (any(W < 0)) stop("All entries of W must by non-negative")
  if (!is.numeric(a)) stop("a must be >= 1")
  if (length(dim(a)) >= 2) stop("a must be a single value")
  if (length(a) > 1) stop("a must be a single value")
  if (a < 1) stop("a must be >= 1")
  if ((a > 1) && (!is.null(b)) && (b <= 0)) stop("b must be strictly positive or NULL when a > 1")
  if (!(is.null(dimnames(W)))) {
    if(length(rownames(W)) != length(colnames(W))) stop("rownames and colnames of W should be the same")
    if(sum(rownames(W) != colnames(W)) > 0) stop("rownames and colnames of W should be the same")
  }

  ### check if provided components appear to match W
  ### NB, this only covers checking the elements of the components match the row/colnames of W,
  ### not whether they are actually the correct components
  if (a == 1 & !is.null(components)) {

    if(!is.list(components)) stop("When a = 1, components should be NULL or a list of components (preferably the saved output of connected_components(W)$components)")

    comp_elements <- unlist(components, use.names = FALSE)
    if(length(comp_elements) != nrow(W)) stop("There are a different number of elements in the components than there are rows/columns in W. Components should be saved output from connected_components(W)$components).")

    if (!is.null(rownames(W))) {
      if (!setequal(comp_elements, rownames(W))) stop("Elements in components do not match the row/colnames of W. Components should be saved output from connected_components(W)$components).")
    }
  }

  ### Save diagonal (for fitted values) then set diagonal of matrix to zero
  saved_diag <- Matrix::diag(W)
  if(!is.null(rownames(W))) names(saved_diag) <- rownames(W)
  diag(W) <- 0

  ### Make sure that matrix is of type dgCMatrix
  if (is.matrix(W)) W <- Matrix(W, sparse = TRUE)
  if (class(W) != "dgCMatrix") W <- as(W, "dgCMatrix")

  ### calculate components, if necessary
  if (a == 1 & is.null(components)) {
    g <- igraph::graph.adjacency(W, weighted = TRUE, diag = FALSE)
    components <- igraph::groups(igraph::components(g, mode = "strong"))
  }

  ### remove components of length 1
  components <- Filter(function(x) length(x) > 1, components)

  ### get necessary dimensions and set up storage
  n <- length(components)
  K <- nrow(W)

  ### Get MLE per component if a = 1
  if (a == 1) {

    ML_method <- match.arg(ML_method)

    # return by component

    pi <- vector("list", n) #  list to hold vectors of pi
    N <- vector("list", n) #  list to hold orginal N matrices
    diagonal <- vector("list", n) #  list to hold diagonal
    iters <- numeric(n) # vector to hold number of iterations per component
    converged <- logical(n)

    for (k in 1:n) {
      compk <- components[[k]] # the players in component k
      Wsub <- W[compk, compk] # wins matrix for players in component k

      if (length(compk) == 2) fit <- list(pi = c(Wsub[1,2], Wsub[2,1])/sum(Wsub), iters = 1, converged = TRUE, N = Wsub + Matrix::t(Wsub))

      else {
        if (ML_method == "ILSR") fit <- ILSR(Wsub, maxit = maxit, epsilon = epsilon)
        if (ML_method == "MM") fit <- BT_EM(Wsub, a = 1, b = 0, maxit = maxit, epsilon = epsilon)
      }

      pi[[k]] <- base::as.vector(fit$pi)
      N[[k]] <- fit$N
      dimnames(N[[k]]) <- dimnames(Wsub)
      names(dimnames(N[[k]])) <- names(dimnames(Wsub))
      diagonal[[k]] <- saved_diag[compk]
      names(pi[[k]]) <- compk
      iters[k] <- fit$iters
      converged[k] <- fit$converged

    }

    # check for convergence problems and provide warning
    if (sum(converged) != n) warning("The algorithm did not converge in at least one component. See the 'converged' element of the output for which.")

  } # end a == 1 if

  ### Get penalised if a > 1
  else {

    if (is.null(b)) b <- a * K - 1

    ### penalised by component, if components are provided
    if (!is.null(components)) {

      pi <- vector("list", n) #  list to hold vectors of pi
      N <- vector("list", n) #  list to hold original N matrices
      diagonal <- vector("list", n) #  list to hold diagonal
      iters <- numeric(n) # vector to hold number of iterations per component
      converged <- logical(n)

      for (k in 1:n) {
        compk <- components[[k]] # the players in component k
        Wsub <- W[compk, compk] # wins matrix for players in component k

        fit <- BT_EM(Wsub, a = a, b = b, maxit = maxit, epsilon = epsilon)

        pi[[k]] <- base::as.vector(fit$pi)
        N[[k]] <- fit$N
        dimnames(N[[k]]) <- dimnames(Wsub)
        names(dimnames(N[[k]])) <- names(dimnames(Wsub))
        diagonal[[k]] <- saved_diag[compk]
        names(pi[[k]]) <- compk
        iters[k] <- fit$iters
        converged[k] <- fit$converged
      } # end loop over components

      # check for convergence problems and provide warning
      if (sum(converged) != n) warning(paste("The algorithm did not converge in maxit = ", maxit, "iterations in at least one component. See the 'converged' element of the output for which."))
    } # end fit by component

    ### EM-algorithm on full W, if no components are provided
    else {

      fit <- BT_EM(W, a = a, b = b, maxit = maxit, epsilon = epsilon)
      pi <- base::as.vector(fit$pi)
      N <- fit$N
      dimnames(N) <- dimnames(W)
      names(dimnames(N)) <- names(dimnames(W))
      iters <- fit$iters
      converged <- fit$converged
      diagonal <- saved_diag

      if (!converged) warning(paste("The algorithm did not converged in maxit =", maxit, "iterations"))
    } # end EM on full W

  } ## end a > 1 if

  # unlist pi, N and diagonal when n = 1
  if (n == 1) {
    pi <- unlist(pi)
    N <- N[[1]]
    diagonal <- unlist(diagonal)
  }

  result <- list(call = call, pi = pi, iters = iters, converged = converged, N = N, diagonal = diagonal)

  class(result) <- c("btfit", "list")

  return(result)
}
