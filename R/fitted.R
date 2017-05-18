my_diag <- function(x,y) {
  diag(x) <- y
  return(x)
}

as_df <- function(sM, N) {

  # get upper triangle
  sM[lower.tri(sM, diag = TRUE)] <- 0
  N[lower.tri(N, diag = TRUE)] <- 0

  if(class(sM) != "dgTMatrix") sM <- methods::as(sM, "dgTMatrix")

  if (!is.null(rownames(sM)) & !is.null(colnames(sM))) {
    #df <- data.frame(winner = rownames(sM)[sM@i + 1], loser = rownames(sM)[sM@j + 1], fit = sM@x)

    df <- data.frame(player1 = rownames(sM)[sM@i + 1], player2 = rownames(sM)[sM@j + 1],
                     fit1 = sM@x, fit2 = N@x - sM@x)
  }

  else {
    df <- data.frame(player1 = sM@i + 1, player2 = sM@j + 1, fit1 = sM@x, fit2 = N@x - sM@x)

  }

  if(!is.null(names(dimnames(N)))) {
    if(!is.na(names(dimnames(N))[1])) colnames(df)[1] <- names(dimnames(N))[1]
    if(!is.na(names(dimnames(N))[2])) colnames(df)[2] <- names(dimnames(N))[2]
  }

  return(df)
}

#' Fitted Method for "btfit"
#'
#' \code{fitted.btfit} returns the fitted values from a fitted brfit model object.
#'
#' Consider a set of \eqn{K} players. Let the players be nodes in a graph and let there be a directed edge \eqn{(i, j)} when \eqn{i} has won against \eqn{j} at least once. We call this the comparison graph of the data, and denote it by \eqn{G_W}. Assuming that \eqn{G_W} is fully connected, the Bradley-Terry model states that the probability that player \eqn{i} beats player \eqn{j} is
#' \deqn{p_{ij} = \frac{\pi_i}{\pi_i + \pi_j},}
#' where \eqn{\pi_i} and \eqn{\pi_j} are positive-valued parameters representing the skills of players \eqn{i} and \eqn{j}, for \eqn{1 \le i, j, \le K}.
#'
#' The expected, or fitted, values under the Bradley-Terry model are therefore:
#'
#' \deqn{m_{ij} = n_{ij}p_{ij},}
#'
#' where \eqn{n_ij} is the number of times player \eqn{i} plays against player \eqn{j}.
#'
#' The function \code{\link{btfit}} is used to fit the Bradley-Terry model. It produces a \code{"btfit"} object that can then be passed to \code{fitted.btfit} to obtain the fitted values \eqn{m_{ij}}. Note that the Bradley-Terry probabilties \eqn{p_{ij}} can be calculated using \code{\link{btprob}}.
#'
#' If \eqn{G_W} is not fully connected, then a penalised strength parameter can be obtained using the method of Caron and Doucet (2012) (see \code{\link{btfit}}, with \code{a > 1}), which allows for a Bradley-Terry probability of any of the K players beating any of the others. Alternatively, the MLE can be found for each fully connected component of \eqn{G_W} (see \code{\link{btfit}}, with \code{a = 1}), and the probability of each player in each component beating any other player in that component can be found.
#' @param ... Other arguments
#' @inheritParams btprob
#'
#' @return If \code{as_df = FALSE}, returns a matrix where the \eqn{i,j}-th element is the Bradley-Terry expected value \eqn{m_{ij}}, or, if \eqn{G_W} is not fully-connected and \code{\link{btfit}} has been run with \code{a = 1}, a list of such matrices for each fully-connected component of \eqn{G_W}. If \code{as_df = TRUE}, returns a four-column data frame, where the first column is \code{player1}, the second column is \code{player2}, the third column, \code{fit1}, is the expected number of times that player 1 beats player 2 and the fourth column, \code{fit2}, is the expected number of times that player 2 beats player 1, (or a list of such data frames for each fully-connected component). If \code{W} has named dimnames, these will be the \code{colnames} for columns one and two. See Details.
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

#'
#'
#' @export
fitted.btfit <- function(object, ..., as_df = FALSE){
  if (!inherits(object, "btfit")) stop("object should be a 'btfit' object")

  # pi <- object$pi
  lambda <- object$lambda
  pi <- exp(lambda)
  N <- object$N
  diagonal <- object$diagonal

  if (is.list(pi)) {
    pi_list_names <- lapply(pi, names)
    out <- Map(fitted_vec, pi, N)
    out <- Map(my_diag, out, diagonal)
    out <- Map(function(x,y) {
      dimnames(x) <- list(y, y)
      return(x)
    }, out, pi_list_names)

    if (as_df) {
      out <- Map(as_df, out, N)
    }

  }

  else {

    pi_names <- names(pi)
    out <- fitted_vec(pi, N)
    diag(out) <- diagonal
    dimnames(out) <- list(pi_names, pi_names)

    if (as_df) out <- as_df(out, N)
  }

  return(out)

}
