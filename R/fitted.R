as_df_fitted <- function(sM, N) {

  # get upper triangle
  sM[lower.tri(sM, diag = TRUE)] <- 0
  N[lower.tri(N, diag = TRUE)] <- 0

  if(class(sM) != "dgTMatrix") sM <- methods::as(sM, "dgTMatrix")

  if (!is.null(rownames(sM)) & !is.null(colnames(sM))) {
    #df <- data.frame(winner = rownames(sM)[sM@i + 1], loser = rownames(sM)[sM@j + 1], fit = sM@x)

    df <- dplyr::tibble(item1 = rownames(sM)[sM@i + 1], item2 = rownames(sM)[sM@j + 1],
                     fit1 = sM@x, fit2 = N@x - sM@x)
  }

  else {
    df <- dplyr::tibble(item1 = sM@i + 1, item2 = sM@j + 1, fit1 = sM@x, fit2 = N@x - sM@x)

  }

  #if(!is.null(names(dimnames(N)))) {
  #  if(!is.na(names(dimnames(N))[1])) colnames(df)[1] <- names(dimnames(N))[1]
  #  if(!is.na(names(dimnames(N))[2])) colnames(df)[2] <- names(dimnames(N))[2]
  #}

  return(df)
}

#' Fitted Method for "btfit"
#'
#' \code{fitted.btfit} returns the fitted values from a fitted brfit model object.
#'
#' Consider a set of \eqn{K} items. Let the items be nodes in a graph and let there be a directed edge \eqn{(i, j)} when \eqn{i} has won against \eqn{j} at least once. We call this the comparison graph of the data, and denote it by \eqn{G_W}. Assuming that \eqn{G_W} is fully connected, the Bradley-Terry model states that the probability that item \eqn{i} beats item \eqn{j} is
#' \deqn{p_{ij} = \frac{\pi_i}{\pi_i + \pi_j},}
#' where \eqn{\pi_i} and \eqn{\pi_j} are positive-valued parameters representing the skills of items \eqn{i} and \eqn{j}, for \eqn{1 \le i, j, \le K}.
#'
#' The expected, or fitted, values under the Bradley-Terry model are therefore:
#'
#' \deqn{m_{ij} = n_{ij}p_{ij},}
#'
#' where \eqn{n_ij} is the number of times item \eqn{i} plays against item \eqn{j}.
#'
#' The function \code{\link{btfit}} is used to fit the Bradley-Terry model. It produces a \code{"btfit"} object that can then be passed to \code{fitted.btfit} to obtain the fitted values \eqn{m_{ij}}. Note that the Bradley-Terry probabilties \eqn{p_{ij}} can be calculated using \code{\link{btprob}}.
#'
#' If \eqn{G_W} is not fully connected, then a penalised strength parameter can be obtained using the method of Caron and Doucet (2012) (see \code{\link{btfit}}, with \code{a > 1}), which allows for a Bradley-Terry probability of any of the K items beating any of the others. Alternatively, the MLE can be found for each fully connected component of \eqn{G_W} (see \code{\link{btfit}}, with \code{a = 1}), and the probability of each item in each component beating any other item in that component can be found.
#' @param ... Other arguments
#' @inheritParams btprob
#'
#' @return If \code{as_df = FALSE} and the model has been fit on the full dataset, returns a matrix where the \eqn{i,j}-th element is the Bradley-Terry expected value \eqn{m_{ij}}. Otherwise, a list of such matrices is returned, one for each fully-connected component. If \code{as_df = TRUE}, returns a five-column data frame, where the first column is \code{item1}, the second column is \code{item2}, the third column, \code{fit1}, is the expected number of times that item 1 beats item 2 and the fourth column, \code{fit2}, is the expected number of times that item 2 beats item 1 and the fifth column is the component that the two items are in. If \code{btdata$wins} has named dimnames, these will be the \code{colnames} for columns one and two. Otherwise these colnames will be \code{item1} and \code{item2}. See Details.
#' @references Bradley, R. A. and Terry, M. E. (1952). Rank analysis of incomplete block designs: 1. The method of paired comparisons. \emph{Biometrika}, \strong{39}(3/4), 324-345.
#' @references Caron, F. and Doucet, A. (2012). Efficient Bayesian Inference for Generalized Bradley-Terry Models. \emph{Journal of Computational and Graphical Statistics}, \strong{21}(1), 174-196.
#' @seealso \code{\link{connected_components}}, \code{\link{btfit}}, \code{\link{btprob}}
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
#' fitted(fit1)
#' fitted(fit2)
#' fitted(fit2, as_df = TRUE)
#' fitted(fit3)
#' @export
fitted.btfit <- function(object, subset = NULL, as_df = TRUE, ...){
  if (!inherits(object, "btfit")) stop("object should be a 'btfit' object")
  
  pi <- object$pi
  N <- object$N
  diagonal <- object$diagonal
  
  #components <- purrr::map(pi, names)
  
  # check and get subset
  if (!is.null(subset)) {
    if (!is.character(subset)) stop("subset should be a character vector")
    if(!all(subset %in% names(pi))) stop("not all elements of subset are names of components")
    pi <- pi[subset]
    N <- N[subset]
    diagonal <- diagonal[subset]
  }
  
  components <- purrr::map(pi, names)
  
  # set up names of dimnames  
  names_dimnames <- object$names_dimnames  
  names_dimnames_list <- list(names_dimnames)
  #names_dimnames_rep <- rep(names_dimnames_list, length(pi))
  
  out <- purrr::map2(pi, N, fitted_vec)
  out <- purrr::map2(out, components, name_matrix_function)
  out <- purrr::map2(out, names_dimnames_list, name_dimnames_function)
  out <- purrr::map2(out, diagonal, my_diag)
  
  if (as_df) {
    comp_names <- names(pi)
    
    out <- purrr::map2(out, N, as_df_fitted)
    
    reps <- purrr::map_int(out, nrow)
    
    out <- purrr::map(out, df_col_rename_func, names_dimnames)
      #purrr::map2(comp_names, ~ .x %>% dplyr::mutate(component = .y)) %>%
    out <- dplyr::bind_rows(out)
    
    
    comps_for_df <- purrr::map2(comp_names, reps, ~rep(.x, each = .y))
    comps_for_df <- unlist(comps_for_df)
    
    out <- dplyr::mutate(out, component = comps_for_df)
    out <- dplyr::select(out, component, 1:4)
  }
    
    if (length(pi) == 1 & !as_df) {
      if (names(pi) == "full_dataset") {
        out <- out[[1]]
      }
    }
  
  out
    
}

