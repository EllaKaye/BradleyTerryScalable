#' Converts a graph representation of wins into a square matrix.
#'
#' \code{graph_to_matrix} is a data manipulation function that converts a graph representation of wins data into a square matrix. The output is of the correct form to be passed as the \code{W} argument into the \code{\link{connected_components}} and \code{\link{btfit}} functions in this package.
#'
#' @param g A directed \code{\link[igraph]{igraph}} object. If \code{g} is unweighted, an edge from node \eqn{i} to \eqn{j} represents one win for player \eqn{i} over player \eqn{j} (and \eqn{n} edges from \eqn{i} to \eqn{j} represents \eqn{n} wins). If \code{g} is weighted, the weight on an edge from node \eqn{i} to \eqn{j} represents the number of times player \eqn{i} has beaten player \eqn{j}.
#' @return A \eqn{K} by \eqn{K} sparse Matrix, where \eqn{K} is the number of players. The \eqn{i,j}-th element of this matrix is the number of times player \eqn{i} beats player \eqn{j}. This output is in the correct format to pass as the \code{W} argument to \code{\link{connected_components}} and \code{\link{btfit}}.
#' @seealso \code{\link{connected_components}}, \code{\link{btfit}}.
#' @examples
#' g <- igraph::make_graph(c(1,2, 1,2, 2,3, 3,4, 5,6), directed = TRUE)
#' graph_to_matrix(g)
#'
#' g2 <- igraph::make_graph(c(1,2, 2,3, 3,4, 5,6), directed = TRUE)
#' igraph::E(g2)$weight <- 1:4
#' graph_to_matrix(g2)

#' @export
graph_to_matrix <- function(g) {

  # check that graph is a directed igraph object
  if(!igraph::is.igraph(g))  stop("g must be a directed igraph object")
  if(!igraph::is.directed(g))  stop("g must be a directed igraph object")
  
  # check names
  if(!is.null(igraph::V(g)$name)) {
    
    arg <- deparse(substitute(g))
    
    if(!identical(length(igraph::V(g)$name), length(unique(igraph::V(g)$name)))) stop(paste0("Vertex names must be unique. Consider fixing with V(", arg, ")$name <- make.names(V(", arg, ")$name, unique = TRUE)"))
  }

  if (igraph::is.weighted(g)) W <- igraph::as_adjacency_matrix(g, sparse = TRUE, attr = "weight", names = TRUE)
  else W <- igraph::as_adjacency_matrix(g, sparse = TRUE, names = TRUE)
  
  #if (igraph::is.weighted(g)) W <- igraph::as_adjacency_matrix(g, sparse = TRUE, attr = "weight")
  #else W <- igraph::as_adjacency_matrix(g, sparse = TRUE)
  

  return(W)

}
