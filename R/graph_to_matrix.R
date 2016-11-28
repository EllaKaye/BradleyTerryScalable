#' @export
graph_to_matrix <- function(g) {

  # check that graph is an igraph or network object (and avoid loading network unless necessary)

  if(!igraph::is.igraph(g)) {
    # Check for network, if required
    if (!requireNamespace("network", quietly = TRUE)) {
      stop("The package 'network' is needed if 'g' is not an igraph object. Please install it.",
           call. = FALSE)
    }

    if(!network::is.network(g)) stop("graph must be an igraph or network object")
  }

  if (igraph::is.igraph(g)) W <- igraph::as_adjacency_matrix(g, sparse = TRUE, attr = "weight")
  if (network::is.network(g)) {
    W <- network::as.matrix.network(g)
    W <- as(W, "dgCMatrix")
  }

  return(W)

}
