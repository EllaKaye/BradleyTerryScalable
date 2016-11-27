#' @export
graph_to_matrix <- function(graph) {

  # check that graph is an igraph or network object (and avoid loading network unless necessary)

  if(!igraph::is.igraph(graph)) {
    # Check for network, if required
    if (!requireNamespace("network", quietly = TRUE)) {
      stop("The package network is needed if 'graph' is not an igraph object. Please install it.",
           call. = FALSE)
    }

    if(!network::is.network(graph)) stop("graph must be an igraph or network object")
  }

  if (igraph::is.igraph(graph)) W <- igraph::as_adjacency_matrix(graph, sparse = TRUE)
  if (network::is.network(graph)) {
    W <- network::as.matrix.network(graph)
    W <- as(W, "dgCMatrix")
  }

  return(W)

}
