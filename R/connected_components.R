#' Checks whether MLE exists for Bradley-Terry model.
#'
#' \code{connected_components} checks whether a unique, finite MLE for the Bradley-Terry model exists. It also returns the fully connected components of the comparison graph of \code{W}, which can be used in \code{\link{btfit}}.
#'
#' As described by Ford (1957), if it is possible to partition the \eqn{K} players into two groups A and B such that there are never any comparisons between players in A and players in B, then there is no basis for rating any player in A with respect to any player in B. Conversely, if all intergroup comparisons are won by a player from the same group, then the likelihood of the Bradley-Terry model has no finite maximiser.
#'
#' Therefore, the following assumption, known as Ford's assumption, must hold for a unique, finite maximiser of the log-likelihood function of the Bradley-Terry model to exist: in every possible partition of players into two non-empty subsets, some individual in the second set beats some individual in the first set at least once.
#'
#' Let the players be nodes in a graph and let there be a directed edge \eqn{(i, j)} when \eqn{i} has won against \eqn{j} at least once. We call this the comparison graph of \code{W}, and denote it by \eqn{G_W}. Ford's assumption is equivalent to saying that there is a path from \eqn{i} to \eqn{j} for all nodes \eqn{i} and \eqn{j}, so that \eqn{G_W} is fully connected.
#'
#' In \code{connected_components}, one of \code{W} or \code{g} must be provided. If the user already has the data represented as an \code{\link[igraph]{igraph}} object, it is faster to supply \code{g} than \code{W}, since if only given \code{W}, \code{connected_components} must converted it to an \code{\link[igraph]{igraph}} object anyway. \code{connected_components} then calls on functions from the \code{\link[igraph]{igraph}} package to check whether Ford's assumption holds for \eqn{G_W}. This will allow the user to decide how they wish to use the \code{\link{btfit}} function.
#'
#' @param W A \eqn{K} by \eqn{K} square wins matrix (e.g. the output of \code{\link{pairs_to_matrix}}), where the \eqn{i,j}-th element is the number of times player \eqn{i} wins against player \eqn{j}, and \eqn{K} is the total number of players. Either \code{W} or \code{g} must be provided (see Details).
#' @param return_components Logical. When \code{TRUE}, the function will return the fully-connected components of the comparison graph \eqn{G_W} (see Details).
#' @param return_graph Logical. When \code{TRUE}, the function will return the comparison graph \eqn{G_W}, an \code{\link[igraph]{igraph}} object (see Details).
#' @inheritParams graph_to_matrix
#' @return Returns a list containing:
#'
#' \item{\code{connected}}{Logical. Returns \code{TRUE} if the comparison graph associated with W is fully connected, \code{FALSE} otherwise.}
#' \item{\code{components}}{If \code{components} is set to \code{TRUE}, this returns a list of the fully connected components of \eqn{G_W}. This can be passed as the \code{components} argument to \code{\link{btfit}}.}
#' \item{\code{graph}}{If \code{graph} is set to \code{TRUE}, this returns the \code{\link[igraph]{igraph}} object of \eqn{G_W}.}
#' @seealso \code{\link{pairs_to_matrix}}, \code{\link{btfit}}.
#' @references Ford, L. R. (1957) Solution of a Ranking Problem from Binary Comparisons. \emph{The American Mathematical Monthly}, \strong{64}(8, Part 2), 28-33.
#' @examples
#' ## A fully connected comparison graph
#' set.seed(1)
#' W <- Matrix::rsparsematrix(10, 10 , 0.7, rand.x = function(n) rbinom(n, 10, 0.5))
#' connected_components(W)$connected
#' ## A comparison graph which is not fully connected
#' set.seed(1)
#' W2 <- Matrix::rsparsematrix(20, 20 , 0.1, rand.x = function(n) rbinom(n, 10, 0.5))
#' connected_components(W2)$components
#' @export
connected_components <- function(W = NULL, g = NULL, return_components = TRUE, return_graph = FALSE) {

  # check that at least one of W and g is entered
  if (is.null(W) & is.null(g)) stop("At least one of W or g must be provided")

  # if g is provided, check it's a igraph
  if(!is.null(g)) {
    if(!igraph::is.igraph(g))  stop("graph must be an igraph or network object")
  }

  # if W is provided and g is not
  if(!is.null(W) & is.null(g)) {
    # check that W is a square matrix of non-negative elements
    if (!(methods::is(W, "Matrix") | is.matrix(W) )) stop("W must be a square matrix")
    if (length(dim(W)) != 2) stop("W must be a square matrix")
    if (dim(W)[1] != dim(W)[2]) stop("W must be a square matrix")
    if (any(W < 0)) stop("All entries of W must by non-negative")

    # convert win matrix into directed graph
    g <- igraph::graph.adjacency(W, weighted = TRUE, diag = FALSE)

  }

  # Check if connected
  connected <- igraph::is.connected(g, mode = "strong")

  # calculate groups of components, if requested
  if (return_components) {
    comp <- igraph::components(g, mode = "strong")
    groups <- igraph::groups(comp)
  }

  # return results
  result <- list(connected = connected)
  if (return_components) result$components <- groups
  if (return_graph) result$graph <- g
  return(result)
}



