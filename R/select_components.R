#' @export

select_components <- function(btdata, comps, return_graph = FALSE) {

  ### Check for correct data object, and extract elements
  if (!inherits(btdata, "btdata")) stop("btdata argument must be a 'btdata' object, as created by btdata() function.")
  
  if (!is.character(comps)) stop("comps should be a character vector")
  if (return_graph & is.null(btdata$graph)) warning("There needs to be a graph component in btdata to return a graph here")
    
  components <- btdata$components
  wins <- btdata$wins
  
  # if there is only one component, there is no selecting to do
  if (length(components) == 1) return(btdata)
  

  # select components
  sub_comps <- components[comps]
  
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
