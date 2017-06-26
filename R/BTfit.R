#' Fits the Bradley-Terry model
#'
#' \code{btfit} fits the Bradley-Terry model on (potentially) large and sparse datasets.
#'
#' Let there be \eqn{K} items, let \eqn{\pi_k} be the Bradley-Terry strength parameter of item \eqn{k}, for \eqn{k = 1, \ldots, K} and let \eqn{\pi} be the vector of all the \eqn{\pi_k}. Let \eqn{w_{ij}} be the number of times item \eqn{i} wins against item \eqn{j}, let \eqn{n_{ij} = w_{ij} + w_{ji}} be the number of times they play, with \eqn{w_{ii} = 0} by convention and let \eqn{W_i = \sum_{j=1}^K w_{ij}}. Then the Bradley-Terry model states that the probability of item \eqn{i} beating item \eqn{j}, \eqn{p_{ij}}, is:
#'
#' \deqn{p_{ij} = \frac{\pi_i}{\pi_i + \pi_j}.}
#' 
#' The comparison graph, \eqn{G_W}, has the \eqn{K} players as the nodes and a directed edge from node \eqn{i} to node \eqn{j} whenever item \eqn{i} has beaten item \eqn{j} at least once. The MLE of the Bradley-Terry model exists and is finite if and only if the comparison graph is fully-connected (i.e. if there is a directed path from node \eqn{i} to node \eqn{j} for all items \eqn{i} and \eqn{j}).
#'
#' Assuming that the comparison graph of the data is fully-connected, the MLE of the Bradley-Terry model can be found using the MM-algorithm (Hunter, 2004).
#'
#' If the comparison graph of the data is not fully-connected, there are two principled options for fitting the Bradley-Terry model. One is to find the MLE within each fully-connected component. The other is to find the Bayesian MAP estimate, as suggested by Caron & Doucet (2012), where a \eqn{Gamma(a,b)}  gamma prior is placed on each \eqn{\pi_i}, and the product of these is taken as a prior on \eqn{\pi}. The MAP estimate can then be found with an EM-algorithm. When \eqn{a = 1} and \eqn{b = 0}, the EM and MM-algorithms are equivalent and the MAP estimate and MLE are identical. The rate parameter of the Gamma prior, \eqn{b}, is not likelihood identifiable. When \eqn{a > 1}, \eqn{b} is set to \eqn{aK - 1}, where \eqn{K} is the number of items in the component; this choice of \eqn{b} minimises the number of iterations needed for the algorithm to converge.
#'
#' The likelihood equations give
#'
#' \deqn{a - 1 + W_i = b\pi_i + \sum_{j \neq i} \frac{n_{ij}\pi_i}{\pi_i + \pi_j},}
#'
#' for \eqn{i = 1, \ldots, K}. For the algorithm to have converged, we want \eqn{\pi} to be such that the LHS and RHS of this equation are close for all \eqn{i}. Therefore, we set the convergence criteria as
#'
#' \deqn{\left|\frac{a - 1 + W_i}{b\pi_i + \sum_{j \neq i} \frac{n_{ij}\pi_i}{\pi_i + \pi_j}} - 1\right| < \epsilon,}
#'
#'for all \eqn{i}.
#'
#' Since the equations do not typeset well within the R help window, we recommend reading this section online: \url{https://ellakaye.github.io/BradleyTerryScalable/reference/btfit.html}.
#'
#' @param a Must be >= 1. When \code{a = 1}, the function returns the MLE estimate of \eqn{\pi} (by component, if necessary). When \code{a > 1}, \code{a} is the shape parameter for the Gamma prior. See Details.
#' @param MAP_by_component Logical. Only considered if a > 1. Then, if FALSE, the MAP estimate will be found on the full dataset. If TRUE, the MAP estimate will be found separately for each fully-connected component.
#' @param maxit The maximum number of iterations for the algorithm. If returning \eqn{\pi} by component, this will be the maximum number of iterations for each component.
#' @param epsilon Determines when the algorithm is deemed to have converged. (See Details.)
#' @inheritParams select_components
#'
#'@return \code{btfit} returns an S3 object of class "btfit". It is a list containing the following components:
#'\item{call}{The matched call}
#'\item{pi}{A list of length \eqn{M}, where \eqn{M} is the number of fully-connected components of the comparison graph \eqn{G_W} (or the requested subset) of two or more items. The \eqn{m}-th list item is a named vector \eqn{\pi}, the strength parameter, for the items in the \eqn{m}-th fully connected component, \eqn{m = 1, \ldots, M}. These are sorted in descending order.}
#'\item{iters}{A vector of length \eqn{M}. The \eqn{m}-th entry is the number of iterations it took for the algorithm to converge for the \eqn{m}-th component, for \eqn{m = 1, \ldots, M}. Note that if the algorithm has not converged in any component, a warning will be produced.}
#'\item{converged}{A logical vector of length \eqn{M}, indicating whether the algorithm has converged for the \eqn{m}-th component in \code{maxit} iterations.}
#'\item{N}{A list of length \eqn{M}. The \eqn{m}-th list item is a matrix where each element \eqn{n_{ij}} is the number of times item \eqn{i} played against item \eqn{j}, for the items in the \eqn{m}-th component. The rows and columns are arranged in the same order as the ordered pi vector(s).}
#'\item{diagonal}{A list of length \eqn{M}. The \eqn{m}-th item is a vector of the diagonal elements of the \code{btdata$wins} matrix, for the items in the \eqn{m}-th fully-connected component}
#'\item{names_dimnames}{The names of the dimnames of the original \code{btdata$wins} matrix.}
#'
#' @seealso \code{\link{btdata}}, \code{\link{summary.btfit}}, \code{\link{coef.btfit}}, \code{\link{fitted.btfit}}, \code{\link{btprob}}, \code{\link{vcov.btfit}}, \code{\link{simulate.btfit}}
#' @author Ella Kaye, David Firth
#'
#' @references Caron, F. and Doucet, A. (2012) Efficient Bayesian Inference for Generalized Bradley-Terry Models. \emph{Journal of Computational and Graphical Statistics}, \strong{21}(1), 174-196.
#' @references Hunter, D. R. (2004) MM Algorithms for Generalized Bradley-Terry Models. \emph{The Annals of Statistics}, \strong{32}(1), 384-406.
#' @examples
#' citations_btdata <- btdata(BradleyTerryScalable::citations)
#' fit1 <- btfit(citations_btdata, 1)
#' summary(fit1)
#' toy_df_4col <- codes_to_counts(BradleyTerryScalable::toy_data, c("W1", "W2", "D"))
#' toy_btdata <- btdata(toy_df_4col)
#' fit2a <- btfit(toy_btdata, 1)
#' summary(fit2a)
#' fit2b <- btfit(toy_btdata, 1.1)
#' summary(fit2b)
#' fit2c <- btfit(toy_btdata, 1, subset = function(x) length(x) > 3)
#' summary(fit2c)
#' @export
btfit <- function(btdata, a, MAP_by_component = FALSE, subset = NULL, maxit = 10000, epsilon = 1e-3) {
  
  call <- match.call()
  
  ### Check for correct data object, and extract elements
  if (!inherits(btdata, "btdata")) stop("btdata argument must be a 'btdata' object, as created by btdata() function.")
  
  orig_components <- btdata$components
  orig_n <- length(orig_components)
  
  if (length(orig_components) == 1 & !is.null(subset)) {
    warning("There is only one component, so subset argument ignored")
    subset <- NULL
  }
  
  if (!is.null(subset)) {
    btdata <- select_components(btdata, subset)
  }
  
  wins <- btdata$wins
  components <- btdata$components
  n <- length(components)
  
  if(!identical(rownames(wins), colnames(wins))) stop("The wins matrix should have identical row and column names")
  
  
  ### Check there's enough data to fit the model
  if (is.numeric(wins)) stop("there is not enough data to fit the model")
  if (nrow(wins) == 1) stop("there is not enough data to fit the model")
  if(is.list(components)) {
    if (sum(purrr::map_int(components, length) > 1) == 0) stop("there is not enough data to fit the model")
  }
  
  ### Check a and b
  if (!is.numeric(a)) stop("a must be >= 1")
  if (length(dim(a)) >= 2) stop("a must be a single value")
  if (length(a) > 1) stop("a must be a single value")
  if (a < 1) stop("a must be >= 1")
  #if ((a > 1) && (!is.null(b))) {
  #  if (!is.numeric(b)) stop("b must be strictly positive or NULL when a > 1")
  #  if ((b <= 0)) stop("b must be strictly positive or NULL when a > 1")
  #} 
  
  ### Save diagonal (for fitted values) then set diagonal of matrix to zero
  saved_diag <- Matrix::diag(wins)
  if(!is.null(rownames(wins))) names(saved_diag) <- rownames(wins)
  diag(wins) <- 0
  
  ### Save names of dimnames (for naming df columns in fitted and btprob)
  names_dimnames <- names(dimnames(wins))
  names_dimnames_list <- list(names_dimnames)
  #names_dimnames_rep <- rep(names_dimnames_list, n)
  
  ### By component, if necessary or by_comp requested
  if ((a == 1 & orig_n > 1) | (a > 1 & MAP_by_component) | (a > 1 & !MAP_by_component & n == 1 & orig_n > 1)) {
    
    ### remove components of length 1
    components <- purrr::discard(components, function(x) length(x) == 1)
    
    # get K and b
    K <- purrr::map_int(components, length)
    if (a == 1) b <- 0
    else b <- a * K - 1
    
    wins_by_comp <- purrr::map(components, ~ wins[.x, .x])
    
    #if (a == 1) btfit_map <- purrr::map(wins_by_comp, BT_EM, a = 1, b = 0, maxit = maxit, epsilon = epsilon)
    # if (a > 1)  btfit_map <- purrr::map(wins_by_comp, BT_EM, a = a, b = b, maxit = maxit, epsilon = epsilon) 
    #if (a > 1)  btfit_map <- purrr::map2(wins_by_comp, b, ~ BT_EM(.x, a = a, b = .y, maxit = maxit, epsilon = epsilon))
    btfit_map <- purrr::map2(wins_by_comp, b, ~ BT_EM(.x, a = a, b = .y, maxit = maxit, epsilon = epsilon))
    # transpose
    btfit_map <- purrr::transpose(btfit_map)
    
    # extract elements and make sure things are properly named
    pi <- purrr::map(btfit_map$pi, as.vector) 
    pi <- purrr::map2(pi, components, name_vec_function)
    N <- purrr::map2(btfit_map$N, components, name_matrix_function)
    N <- purrr::map2(N, names_dimnames_list, name_dimnames_function)
    iters <- unlist(btfit_map$iters)
    converged <- unlist(btfit_map$converged)
    
    # diagonal
    diagonal <- purrr::map(components, ~ saved_diag[.x]) 
    diagonal <- purrr::map2(diagonal, components, name_vec_function)    
    
    # check for convergence problems and provide warning
    n <- length(components)
    if (sum(converged) != n) warning("The algorithm did not converge in at least one component. See the 'converged' element of the output for which.")    
  }
  
  ### Or on whole matrix
  else {
    K <- nrow(wins)
    if (a == 1) b <- 0
    else b <- a * K - 1
    
    fit <- BT_EM(wins, a = a, b = b, maxit = maxit, epsilon = epsilon)
    #if(a > 1) fit <- BT_EM(wins, a = a, b = b, maxit = maxit, epsilon = epsilon)
    pi <- base::as.vector(fit$pi)
    names(pi) <- rownames(wins)
    pi <- list(pi)
    N <- fit$N
    dimnames(N) <- dimnames(wins)
    names(dimnames(N)) <- names(dimnames(wins))
    N <- list(N)
    iters <- fit$iters
    converged <- fit$converged
    diagonal <- list(saved_diag)
    names(pi) <- names(N) <- names(diagonal) <- "full_dataset"
    
    if (!converged) warning(paste("The algorithm did not converged in maxit =", maxit, "iterations"))
  }
  
  if (a > 1 & MAP_by_component & orig_n == 1) names(pi) <- "full_dataset"
  
  # reorder
  pi_perm <- purrr::map(pi, order, decreasing = TRUE)
  pi <- purrr::map2(pi, pi_perm, ~.x[.y])
  N <- purrr::map2(N, pi_perm, ~.x[.y, .y])
  diagonal <- purrr::map2(diagonal, pi_perm, ~.x[.y])    
  
  
  result <- list(call = call, pi = pi, iters = iters, converged = converged, N = N,
                 diagonal = diagonal, names_dimnames = names_dimnames)
  
  class(result) <- c("btfit", "list")
  
  return(result)
}
