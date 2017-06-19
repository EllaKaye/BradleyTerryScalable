as_df_btprob <- function(m) {
  
  # convert to matrix
  if (!is.matrix(m)) m <- as.matrix(m)
  
  # Check for reshape2
  #if (!requireNamespace("reshape2", quietly = TRUE)) {
  #  stop("The package reshape2 is needed for as_df = TRUE in btprob. Please install it.",
  #       call. = FALSE)
  #}
  
  m[lower.tri(m, diag = TRUE)] <- NA
  
  # hack to avoid NOTES on global variables
  #value <- NULL
  #prob1wins <- NULL
  
  # make the data frame  
  out <- dplyr::as_data_frame(as.data.frame.table(m, useNA = "no", stringsAsFactors = FALSE))
  out <- dplyr::filter(out, !is.na(Freq))
  #out <- dplyr::as_data_frame(reshape2::melt(m, na.rm = TRUE)) 
  #out <- dplyr::mutate_if(out, is.factor, as.character)
  #out <- dplyr::rename(out, prob1wins = value)
  out <- dplyr::rename(out, prob1wins = Freq)
  out <- dplyr::mutate(out, prob2wins = 1 - as.numeric(prob1wins))
  
  out
}

#' Calculates Bradley-Terry probabilities
#'
#' Calculates the Bradley-Terry probabilities of each item in a fully-connected component of \eqn{G_W} winning against every other item in that component (see Details).
#'
#' Consider a set of \eqn{K} items. Let the items be nodes in a graph and let there be a directed edge \eqn{(i, j)} when \eqn{i} has won against \eqn{j} at least once. We call this the comparison graph of the data, and denote it by \eqn{G_W}. Assuming that \eqn{G_W} is fully connected, the Bradley-Terry model states that the probability that item \eqn{i} beats item \eqn{j} is
#' \deqn{p_{ij} = \frac{\pi_i}{\pi_i + \pi_j},}
#' where \eqn{\pi_i} and \eqn{\pi_j} are positive-valued parameters representing the skills of items \eqn{i} and \eqn{j}, for \eqn{1 \le i, j, \le K}. The function \code{\link{btfit}} can be used to find the strength parameter \eqn{\pi}. It produces a \code{"btfit"} object that can then be passed to \code{btprob} to obtain the Bradley-Terry probabilities \eqn{p_{ij}}.
#'
#' If \eqn{G_W} is not fully connected, then a penalised strength parameter can be obtained using the method of Caron and Doucet (2012) (see \code{\link{btfit}}, with \code{a > 1}), which allows for a Bradley-Terry probability of any of the K items beating any of the others. Alternatively, the MLE can be found for each fully connected component of \eqn{G_W} (see \code{\link{btfit}}, with \code{a = 1}), and the probability of each item in each component beating any other item in that component can be found.
#'
#' @param object An object of class "btfit", typically the result \code{ob} of \code{ob <- btfit(..)}. See \code{\link{btfit}}.
#' @param as_df Logical scalar, determining class of output. If \code{TRUE}, the function returns a data frame. If \code{FALSE} (the default), the function returns a matrix (or list of matrices).
#'@param subset A condition for selecting a subset of the components. This can either be a character vector of names of the components (i.e. a subset of \code{names(object$pi)}), a single predicate function (that takes a vector of \code{object$pi} as its argument), or a logical vector of the same length as the number of components, (i.e. \code{length(object$pi)}).
#' @return If \code{as_df = FALSE}, returns a matrix where the \eqn{i,j}-th element is the Bradley-Terry probability \eqn{p_{ij}}, or, if \eqn{G_W} is not fully-connected and \code{\link{btfit}} has been run with \code{a = 1}, a list of such matrices for each fully-connected component of \eqn{G_W}. If \code{as_df = TRUE}, returns a five-column data frame, where the first column is the component that the two items are in, the second column is \code{item1}, the third column is \code{item2}, the fourth column is the Bradley-Terry probability that item 1 beats item 2 and the fifth column is the Bradley-Terry probability that item 2 beats item 1. If the original \code{btdata$wins} matrix has named dimnames, these will be the \code{colnames} for columns one and two. See Details.
#' @references Bradley, R. A. and Terry, M. E. (1952). Rank analysis of incomplete block designs: 1. The method of paired comparisons. \emph{Biometrika}, \strong{39}(3/4), 324-345.
#' @references Caron, F. and Doucet, A. (2012). Efficient Bayesian Inference for Generalized Bradley-Terry Models. \emph{Journal of Computational and Graphical Statistics}, \strong{21}(1), 174-196.
#' @seealso \code{\link{btfit}}, \code{\link{btdata}}
#' @examples
#' W_connected <- Matrix::rsparsematrix(10, 10 , 0.5, rand.x = function(n) rbinom(n, 10, 0.5))
#' i <- c(3,1,5,4,2,5,5,7,8,5,6,8,7)
#' j <- c(1,2,2,3,4,4,6,6,6,7,7,7,8)
#' dimnames = list(letters[1:8], letters[1:8])
#' W_not_connected <-  Matrix::sparseMatrix(i, j, x = 1:13, dims = c(8,8), dimnames = dimnames)
#' W_connected_data <- btdata(W_connected)
#' W_not_connected_data <- btdata(W_not_connected)
#' fit1 <- btfit(W_connected_data, 1)
#' fit2 <- btfit(W_not_connected_data, 1)
#' fit3 <- btfit(W_not_connected_data, 3)
#' btprob(fit1)
#' btprob(fit2)
#' btprob(fit2, as_df = TRUE)
#' btprob(fit3)
#' @export
btprob <- function(object, subset = NULL, as_df = FALSE) {
  
  if (!inherits(object, "btfit")) stop("Object should be a 'btfit' object")
  
  pi <- object$pi
  
  # check and get subset
  if (!is.null(subset)) {
    pi <- subset_by_pi(pi, subset)
  }
  
  components <- purrr::map(pi, names)
  
  # set up names of dimnames  
  names_dimnames <- object$names_dimnames  
  names_dimnames_list <- list(names_dimnames)
  #names_dimnames_rep <- rep(names_dimnames_list, length(pi))
  
  p <- purrr::map(pi, btprob_vec)
  p <- purrr::map2(p, components, name_matrix_function)
  p <- purrr::map2(p, names_dimnames_list, name_dimnames_function)
  
  if (as_df) {
    comp_names <- names(pi)
    
    p <- purrr::map(p, as_df_btprob) 
    
    reps <- purrr::map_int(p, nrow)
    
    p <- purrr::map(p, df_col_rename_func, names_dimnames)
    #p <- purrr::map2(p, comp_names, ~ .x %>% dplyr::mutate(component = .y))
    p <- dplyr::bind_rows(p)
    
    
    comps_for_df <- purrr::map2(comp_names, reps, ~rep(.x, each = .y))
    comps_for_df <- unlist(comps_for_df)
    
    p <- dplyr::mutate(p, component = comps_for_df)
    
    # hack to avoid CRAN note
    component <- NULL
    
    p <- dplyr::select(p, component, 1:4)
  }
  
  if (length(pi) == 1 & !as_df) {
    if (names(pi) == "full_dataset") {
      p <- p[[1]]
    }
  }
  
  p
}