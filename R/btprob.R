as_df_prob <- function(m) {

  # Check for reshape2
  if (!requireNamespace("reshape2", quietly = TRUE)) {
    stop("The package reshape2 is needed for as_df = TRUE in btprob. Please install it.",
         call. = FALSE)
  }

  m[lower.tri(m, diag = TRUE)] <- NA
  out <- reshape2::melt(m, na.rm = TRUE)
  colnames(out)[3] <- "prob1wins"
  out$prob2wins <- 1 - out$prob1wins

  out
}

rename_func <- function(df, names_dimnames) {

  colnames(df)[1:2] <- c("player1", "player2")

  if(!is.null(names_dimnames)) {
    if(!is.na(names_dimnames[1])) colnames(df)[1] <- names_dimnames[1]
    if(!is.na(names_dimnames[2])) colnames(df)[2] <- names_dimnames[2]
  }

  return(df)
}

#' Calculates Bradley-Terry probabiities
#'
#' Calculates the Bradley-Terry probabilities of each player in a fully-connected component of \eqn{G_W} winning against every other player in that component.
#'
#' Consider a set of \eqn{K} players. Let the players be nodes in a graph and let there be a directed edge \eqn{(i, j)} when \eqn{i} has won against \eqn{j} at least once. We call this the comparison graph of the data, and denote it by \eqn{G_W}. Assuming that \eqn{G_W} is fully connected, the Bradley-Terry model states that the probability that player \eqn{i} beats player \eqn{j} is
#' \deqn{p_{ij} = \frac{\pi_i}{\pi_i + \pi_j},}
#' where \eqn{\pi_i} and \eqn{\pi_j} are positive-valued parameters representing the skills of players \eqn{i} and \eqn{j}, for \eqn{1 \le i, j, \le K}. The function \code{\link{btfit}} can be used to find the strength parameter \eqn{\pi}. It produces a \code{"btfit"} object that can then be passed to \code{btprob} to obtain the Bradley-Terry probabilities \eqn{p_{ij}}.
#'
#' If \eqn{G_W} is not fully connected, then a penalised strength parameter can be obtained using the method of Caron and Doucet (2012) (see \code{\link{btfit}}, with \code{a > 1}), which allows for a Bradley-Terry probability of any of the K players beating any of the others. Alternatively, the MLE can be found for each fully connected component of \eqn{G_W} (see \code{\link{btfit}}, with \code{a = 1}), and the probability of each player in each component beating any other player in that component can be found.
#'
#' @param object An object of class "btfit", typically the result \code{ob} of \code{ob <- btfit(..)}. See \code{\link{btfit}}.
#' @param as_df Logical scalar, determining class of output. If \code{TRUE}, the function returns a data frame (or list of data frames). If \code{FALSE} (the default), the function returns a matrix (or list of matrices).
#'
#' @return If \code{as_df = FALSE}, returns a matrix where the \eqn{i,j}-th element is the Bradley-Terry probability \eqn{p_{ij}}, or, if \eqn{G_W} is not fully-connected and \code{\link{btfit}} has been run with \code{a = 1}, a list of such matrices for each fully-connected component of \eqn{G_W}. If \code{as_df = TRUE}, returns a four-column data frame, where the first column is \code{player1}, the second column is \code{player2}, the third column is the Bradley-Terry probability that player 1 beats player 2 and the fourth column is the Bradley-Terry probability that player 2 beats player 1, (or a list of such data frames for each fully-connected component). If \code{W} has named dimnames, these will be the \code{colnames} for columns one and two. See Details.
#' @references Bradley, R. A. and Terry, M. E. (1952). Rank analysis of incomplete block designs: 1. The method of paired comparisons. \emph{Biometrika}, \strong{39}(3/4), 324-345.
#' @references Caron, F. and Doucet, A. (2012). Efficient Bayesian Inference for Generalized Bradley-Terry Models. \emph{Journal of Computational and Graphical Statistics}, \strong{21}(1), 174-196.
#' @seealso \code{\link{connected_components}}, \code{\link{btfit}}, \code{\link{fitted.btfit}}
#' @examples
#' W_connected <- Matrix::rsparsematrix(10, 10 , 0.5, rand.x = function(n) rbinom(n, 10, 0.5))
#' i <- c(3,1,5,4,2,5,5,7,8,5,6,8,7)
#' j <- c(1,2,2,3,4,4,6,6,6,7,7,7,8)
#' dimnames = list(letters[1:8], letters[1:8])
#' W_not_connected <-  Matrix::sparseMatrix(i, j, x = 1:13, dims = c(8,8), dimnames = dimnames)
#' W_components <- connected_components(W_not_connected)$components
#' fit1 <- btfit(W_connected, a = 1)
#' fit2 <- btfit(W_not_connected, 1, components = W_components)
#' fit3 <- btfit(W_not_connected, 3)
#' btprob(fit1)
#' btprob(fit2)
#' btprob(fit2, as_df = TRUE)
#' btprob(fit3)
#' @export


btprob <- function(object, as_df = FALSE) {

  #if(!is.vector(pi)) stop("pi should be a numeric vector or a list of numeric vectors")
  #if(!is.list(pi) & !is.numeric(pi)) stop("pi should be a numeric vector or a list of numeric vectors")
  #if(is.list(pi) & (sum(sapply(pi, is.numeric)) != length(pi))) stop("pi should be a numeric vector or a list of numeric vectors")

  if (!inherits(object, "btfit")) stop("Object should be a 'btfit' object")

  # pi <- object$pi
  lambda <- object$lambda
  pi <- exp(lambda)
  names_dimnames <- object$names_dimnames

  if (is.list(pi)) {
    pi_list_names <- lapply(pi, names)
    unnamed <- lapply(pi, btprob_vec)
    p <- Map(function(x,y) {
      dimnames(x) <- list(y, y)
      return(x)
    }, unnamed, pi_list_names)

    if (as_df) {
      p <- lapply(p, as_df_prob)
      p <- lapply(p, rename_func, names_dimnames)
    }
  }

  else {
    pi_names <- names(pi)
    p <- btprob_vec(pi)
    dimnames(p) <- list(pi_names, pi_names)

    if(as_df) {
      p <- as_df_prob(p)
      p <- rename_func(p, names_dimnames)
    }
  }

  p
}


