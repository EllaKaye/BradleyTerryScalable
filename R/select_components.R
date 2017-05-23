#' @export

select_components <- function(btdata, comps) {

  ### Check for correct data object, and extract elements
  if (!inherits(btdata, "btdata")) stop("btdata argument must be a 'btdata' object, as created by btdata() function.")
    
  components <- btdata$components
  wins <- btdata$wins
  
  # if there is only one component, there is no selecting to do
  if (length(components) == 1) return(btdata)
  

  # select components
  sub_comps <- components[comps]
  
  # subset wins
  sub_wins <- wins[unlist(sub_comps), unlist(sub_comps)] 
    
  # subset graph
  #if (!is.null(btdata$graph)) {
    
  #}
  
  result <- list(wins = sub_wins, components = sub_comps)
  # if (!is.null(btdata$graph)) result$graph <- g
  class(result) <- c("btdata", "list")
  result  
}
