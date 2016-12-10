#' @export
graph_to_matrix <- function(g) {

  # check that graph is an igraph object

  if(!igraph::is.igraph(g))  stop("g must be an igraph object")

  if (igraph::is.weighted(g)) W <- igraph::as_adjacency_matrix(g, sparse = TRUE, attr = "weight")
  else W <- igraph::as_adjacency_matrix(g, sparse = TRUE)

  return(W)

}
