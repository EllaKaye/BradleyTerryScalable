#' Subset a btdata object
#' 
#' Subset a btdata object by selecting components from it.
#' 
#' @param btdata A \code{\link{btdata}} object
#' @param subset A condition for selecting a subset of the components. This can either be a character vector of names of the components, a single predicate function (that takes a component as its argument), or a logical vector of the same length as the number of components).
#' @inheritParams btdata
#' @return A \code{\link{btdata}} object, which is a list containing:
#' \item{wins}{A square matrix, where the \eqn{i,j}-th element is the number of times item \eqn{i} has beaten item \eqn{j}.}
#' \item{components}{A list of the fully-connected components. The names of the list preserve the names of the original \code{btdata} object.}
#' \item{graph}{The comparison graph of the data (if return_graph = TRUE).}
#' @seealso \code{\link{btdata}}, \code{\link{btfit}}
#' @export
select_components <- function(btdata, subset, return_graph = FALSE) {

  ### Check for correct data object, and extract elements
  if (!inherits(btdata, "btdata")) stop("btdata argument must be a 'btdata' object, as created by btdata() function.")
  
  if (return_graph & is.null(btdata$graph)) warning("There needs to be a graph component in btdata to return a graph here")
    
  components <- btdata$components
  wins <- btdata$wins
  
  # check that subset is of the correct form
  if (!is.function(subset) & !is.character(subset) & !(is.logical(subset))) stop(
    "subset is not of the correct form - see the documentation for more details."
  )
  
  if(is.logical(subset)) {
    if (length(subset) != length(components)) stop("length(subset) == length(btdata$components) is not TRUE")
  }
  
  # quick check on subset if function (on component 1)
  if(is.function(subset)) {
    test_of_function <- subset(components[[1]])
    if (!is.logical(test_of_function)) stop("if subset is a function, it must return either TRUE or FALSE")
    if (length(test_of_function) > 1) stop("if subset is a function, it must return either TRUE or FALSE")
  }
  # rely on purrr::keep's error message if length == 1 on comp 1, but > 1 elsewhere
  
  # correct way to test subset on all components (but may be too costly)
  # if (purrr::map(components, subset) %>% map_int(length) %>% any(. > 1)) stop("if subset is a function, it must return either TRUE or FALSE")
  
  if(is.character(subset)) {
    if(!all(subset %in% names(components))) stop("not all elements of subset are names of components")
  }

  # select components
  if (is.character(subset)) sub_comps <- components[subset]
  else sub_comps <- purrr::keep(components, subset)
  
  # subset wins
  sub_wins <- wins[unlist(sub_comps), unlist(sub_comps)] 
    
  # subset graph
  if (!is.null(btdata$graph) & return_graph) {
    graph <- btdata$graph
    g <- igraph::induced_subgraph(graph, unlist(sub_comps))
  }
  
  result <- list(wins = sub_wins, components = sub_comps)
  if (!is.null(btdata$graph) & return_graph) result$graph <- g
  class(result) <- c("btdata", "list")
  result  
}
