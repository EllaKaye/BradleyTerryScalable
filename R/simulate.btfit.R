#' @export
simulate_vec <- function(pi, N, nsim = 1, seed = NULL){

  ## A simulate function that takes a vector pi and a matrix N as its arguments
  
  # set the seed
  if (!is.null(seed)) set.seed(seed)
  
  # simulate some data (including dealing with nsim...)
  
}

#' @export
simulate.btfit <- function(object, nsim = 1, seed = NULL, ...){
  
  if (!inherits(object, "btfit")) stop("object should be a 'btfit' object")
  
  pi <- object$pi
  N <- object$N
  
  # iterate over components
  if (is.list(pi)) result <- purrr::map2(pi, N, simulate_vec, nsim = nsim, seed = seed)
  
  # or single vector
  else result <- simulate_vec(pi, N, nsim = nsim, seed = seed) 
  
  result
}
