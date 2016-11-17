#' Checks whether MLE exists for Bradley-Terry model.
#'
#' \code{MLE_check} checks whether a unique, finite MLE for the Bradley-Terry model exists.
#'
#' As described by Ford (1957), if it is possible to partition the players into two groups A and B such that there are never any comparisons between players in A and players in B, then there is no basis for rating any player in A with respect to any player in B. Conversely, if all intergroup comparisons are won by a player from the same group, then the likelihood has no finite maximiser.
#'
#' Therefore, the following assumption, known as Ford's assumption, must hold for a unique, finite maximiser of the log-likelihood function of the Bradley-Terry model to exist: in every possible partition of players into two non-empty subsets, some individual in the second set beats some individual in the first set at least once.
#'
#' Let the players be nodes in a graph, and allow a directed edge \eqn{(i, j)} to denote a win by \eqn{i} over \eqn{j}. We call this the comparison graph of \code{W}. Ford's assumption is equivalent to saying that there is a path from \eqn{i} to \eqn{j} for all nodes \eqn{i} and \eqn{j}, so that the comparison graph is strongly connected.
#'
#' \code{\link{MLE_check}} calls on functions from the \code{\link[igraph]{igraph}} package to check whether Ford's assumption holds on the comparison graph of \code{W}. Based on the output information, the user can chose which of the other functions in this package, \code{\link{BT_EM}}, \code{\link{ILSR}} or \code{\link{BT_comp}} best meets their model-fitting needs.
#'
#' @param W A \eqn{K} by \eqn{K} square wins matrix (e.g. the output of \code{\link{pairs_to_matrix}}), where the \eqn{i,j}-th element is the number of times player \eqn{i} beats player \eqn{j}, and \eqn{K} is the total number of players.
#' @param graph Logical - when \code{TRUE}, the function will return the comparison graph, an \code{\link[igraph]{igraph}} object
#' @param components Logical - when \code{TRUE}, the function will return the fully-connected components of the comparison graph
#' @return Returns a list containing:
#'
#' \item{\code{connected}}{Logical. Returns \code{TRUE} if the comparison graph associated with W is fully connected, \code{FALSE} otherwise.}
#' \item{\code{graph}}{If \code{graph} is set to \code{TRUE}, this returns the directed \code{\link[igraph]{igraph}} object associated with W. This can be passed as the \code{graph} argument to \code{\link{BT_comp}}.}
#' \item{\code{components}}{If \code{components} is set to \code{TRUE}, this returns the groups of fully connected components of the graph associated with \code{W}. This can be passed as the \code{components} argument to \code{\link{BT_comp}}.}
#' @seealso \code{\link{pairs_to_matrix}}, \code{\link{ILSR}}, \code{\link{BT_EM}}, \code{\link{BT_comp}}
#' @examples
#' ## A fully connected comparison graph
#' set.seed(1)
#' W <- Matrix::rsparsematrix(10, 10 , 0.7, rand.x = function(n) rbinom(n, 10, 0.5))
#' MLE_check(W)$connected
#' ## A comparison graph which is not fully connected
#' set.seed(1)
#' W2 <- Matrix::rsparsematrix(20, 20 , 0.1, rand.x = function(n) rbinom(n, 10, 0.5))
#' MLE_check(W2)$components
#' @export
connected_components <- function(W, components = TRUE, graph = FALSE) {

  # check that W is a square matrix of non-negative elements
  if (length(dim(W)) != 2) stop("W must be a square matrix")
  if (dim(W)[1] != dim(W)[2]) stop("W must be a square matrix")
  if (any(W < 0)) stop("All entries of W must by non-negative")

  # convert win matrix into directed graph and check if connected
  g <- igraph::graph.adjacency(W, weighted = TRUE, diag = FALSE)
  connected <- igraph::is.connected(g, mode = "strong")

  # calculate groups of components, if requested
  if (components) {
    comp <- igraph::components(g, mode = "strong")
    groups <- igraph::groups(comp)
  }

  # return results
  result <- list(connected = connected)
  if (components) result$components <- groups
  if (graph) result$graph <- g
  return(result)
}



