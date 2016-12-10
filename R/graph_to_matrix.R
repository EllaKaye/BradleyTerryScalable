#' @export
graph_to_matrix <- function(g) {

  # check that graph is an igraph object

  if(!igraph::is.igraph(g))  stop("graph must be an igraph or network object")

  if (igraph::is.igraph(g)) W <- igraph::as_adjacency_matrix(g, sparse = TRUE, attr = "weight")

  return(W)

}
