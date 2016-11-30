#' Calculates Bradley-Terry probabiities
#'
#' Given the strength parameter from fitting the Bradley-Terry model, (e.g. the \code{pi} output of \code{\link{btfit}}), calculates the Bradley-Terry probabilities of each player in a fully-connected component of \eqn{G_W} winning against every other player in that component.
#'
#' Consider a set of \eqn{K} players. Let the players be nodes in a graph and let there be a directed edge \eqn{(i, j)} when \eqn{i} has won against \eqn{j} at least once. We call this the comparison graph of the data, and denote it by \eqn{G_W}. Assuming that \eqn{G_W} is fully connected, the Bradley-Terry model states that the probability that player \eqn{i} beats player \eqn{j} is
#' \deqn{p_{ij} = \frac{\pi_i}{\pi_i + \pi_j},}
#' where \eqn{\pi_i} and \eqn{\pi_j} are positive-valued parameters representing the skills of players \eqn{i} and \eqn{j}, for \eqn{1 \le i, j, \le K}. The function \code{\link{btfit}} can be used to find the strength parameter \eqn{\pi} and, given that, \code{btprobs} returns the \eqn{p_ij}.
#'
#' If \eqn{G_W} is not fully connected, then a penalised strength parameter can be obtained using the method of Caron and Doucet (2012) (see \code{\link{btfit}}, with \code{a > 1}), which allows for a Bradley-Terry probability of any of the K players beating any of the others. Alternatively, the MLE can be found for each fully connected component of \eqn{G_W} (see \code{\link{btfit}}, with \code{a = 1}), and the probability of each player in each component beating any other player in that component can be found.
#'
#' @param pi A numeric vector of Bradley-Terry strength parameters, or a list of numeric vectors of strength parameters by component. This should be the \code{pi} output of \code{\link{btfit}}.
#'
#' @return If \code{pi} is a single vector of all \eqn{K} players, returns a \eqn{K} by \eqn{K} matrix where the \eqn{(i,j)}-th element is the Bradley-Terry probability that player \eqn{i} wins against player \eqn{j}. If \code{pi} is a list of strength parameters by component, returns a list of such matrices, one for each component.
#' @references Bradley, R. A. and Terry, M. E. (1952). Rank analysis of incomplete block designs: 1. The method of paired comparisons. \emph{Biometrika}, \strong{39}(3/4), 324-345.
#' @references Caron, F. and Doucet, A. (2012). Efficient Bayesian Inference for Generalized Bradley-Terry Models. \emph{Journal of Computational and Graphical Statistics}, \strong{21}(1), 174-196.
#' @seealso \code{\link{connected_components}}, \code{\link{btfit}}
#' @examples
#' W_connected <- Matrix::rsparsematrix(10, 10 , 0.5, rand.x = function(n) rbinom(n, 10, 0.5))
#' i <- c(3,1,5,4,2,5,5,7,8,5,6,8,7)
#' j <- c(1,2,2,3,4,4,6,6,6,7,7,7,8)
#' W_not_connected <-  Matrix::sparseMatrix(i = i, j = j, x = 1:13, dims = c(8,8), dimnames = list(letters[1:8], letters[1:8]))
#' W_components <- connected_components(W_not_connected)$components
#' pi1 <- btfit(W_connected, 1)$pi
#' pi2 <- btfit(W_not_connected, 1, components = W_components)$pi
#' pi3 <- btfit(W_not_connected, 3)$pi
#' btprob(pi1)
#' btprob(pi2)
#' btprob(pi3)
#' @export


btprob <- function(pi) {

  if(!is.vector(pi)) stop("pi should be a numeric vector or a list of numeric vectors")
  if(!is.list(pi) & !is.numeric(pi)) stop("pi should be a numeric vector or a list of numeric vectors")
  if(is.list(pi) & (sum(sapply(pi, is.numeric)) != length(pi))) stop("pi should be a numeric vector or a list of numeric vectors")


  if (is.list(pi)) {
    pi_list_names <- lapply(pi, names)
    unnamed <- lapply(pi, btprob_vec)
    p <- Map(function(x,y) {
      dimnames(x) <- list(y, y)
      return(x)
    }, unnamed, pi_list_names)
  }

  else {
    pi_names <- names(pi)
    p <- btprob_vec(pi)
    dimnames(p) <- list(pi_names, pi_names)
  }

  p
}


