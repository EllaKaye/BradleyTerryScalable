name_vec_function <- function(x, y) {
  names(x) <- y 
  return(x)
}

name_matrix_function <- function(x, y) {
  dimnames(x) <- list(y,y)
  return(x)
}

name_dimnames_function <- function(x) {
  names(dimnames(x)) <- names_dimnames
  return(x)
}

#' @export
btfit2 <- function(btdata, a, b = NULL, MAP_by_component = FALSE, subset = NULL, maxit = 10000, epsilon = 1e-3) {
  
  call <- match.call()
  
  ### Check for correct data object, and extract elements
  if (!inherits(btdata, "btdata")) stop("btdata argument must be a 'btdata' object, as created by btdata() function.")

  wins <- btdata$wins
  components <- btdata$components
  
  if (length(components) == 1 & !is.null(subset)) {
    warning("There is only one component, so subset argument ignored")
    subset <- NULL
  }
    
  if (!is.null(subset)) {
    btdata <- select_components(btdata, subset)
  }
  
  wins <- btdata$wins
  components <- btdata$components
  
  ### Check there's enough data to fit the model
  if (is.numeric(wins)) stop("there is not enough data to fit the model")
  if (nrow(wins) == 1) stop("there is not enough data to fit the model")
  if(is.list(components)) {
    if (sum(purrr::map_int(components, length) > 1) == 0) stop("there is not enough data to fit the model")
  }
  
  ### Check a and b
  if (!is.numeric(a)) stop("a must be >= 1")
  if (length(dim(a)) >= 2) stop("a must be a single value")
  if (length(a) > 1) stop("a must be a single value")
  if (a < 1) stop("a must be >= 1")
  if ((a > 1) && (!is.null(b))) {
    if (!is.numeric(b)) stop("b must be strictly positive or NULL when a > 1")
    if ((b <= 0)) stop("b must be strictly positive or NULL when a > 1")
  } 
  
  ### Save diagonal (for fitted values) then set diagonal of matrix to zero
  saved_diag <- Matrix::diag(wins)
  if(!is.null(rownames(wins))) names(saved_diag) <- rownames(wins)
  diag(W) <- 0
  
  ### Save names of dimnames (for naming df columns in fitted and btprob)
  names_dimnames <- names(dimnames(wins))
  
  ### remove components of length 1
  components <- purrr::discard(components, function(x) length(x) == 1)
  
    
  ### get necessary dimensions and set up storage
  n <- length(components)
  K <- nrow(W)
  if (is.null(b)) b <- a * K - 1
  
  ### By component, if necessary or by_comp requested
  if ((a == 1 & n > 1) | (a > 1 & MAP_by_component)) {
    
    wins_by_comp <- purrr::map(components, ~ wins[.x, .x])
    
    if (a == 1) btfit_map <- purrr::map(wins_by_comp, BT_EM, a = 1, b = 0, maxit = maxit, epsilon = epsilon)
    if (a > 1)  btfit_map <- purrr::map(wins_by_comp, BT_EM, a = a, b = b, maxit = maxit, epsilon = epsilon)     
    # transpose
    btfit_map <- purrr::transpose(btfit_map)
    
    # extract elements and make sure things are properly named
    pi <- purrr::map(btfit_map$pi, as.vector) %>%
      purrr::map2(components, name_vec_function)
    N <- purrr::map2(btfit_map$N, components, name_matrix_function) %>%
      purrr::map(name_dimnames_function)
    iters <- unlist(btfit_map$iters)
    converged <- unlist(btfit_map$converged)

    # diagonal
    diagonal <- purrr::map(components, ~ saved_diag[.x]) %>%
      purrr::map2(components, name_vec_function)      
    
    # check for convergence problems and provide warning
    if (sum(converged) != n) warning("The algorithm did not converge in at least one component. See the 'converged' element of the output for which.")    
  }
    
  ### Or on whole matrix
  else {
    if(a == 1) fit <- BT_EM(wins, a = 1, b = 0, maxit = maxit, epsilon = epsilon)
    if(a > 1) fit <- BT_EM(wins, a = a, b = b, maxit = maxit, epsilon = epsilon)
    pi <- base::as.vector(fit$pi)
    names(pi) <- rownames(wins)
    pi <- list(pi)
    N <- fit$N
    dimnames(N) <- dimnames(wins)
    names(dimnames(N)) <- names(dimnames(wins))
    N <- list(N)
    iters <- fit$iters
    converged <- fit$converged
    diagonal <- list(saved_diag)
    
    if (!converged) warning(paste("The algorithm did not converged in maxit =", maxit, "iterations"))
  }
  
  result <- list(call = call, pi = pi, iters = iters, converged = converged, N = N,
                 diagonal = diagonal, names_dimnames = names_dimnames)
  
  class(result) <- c("btfit", "list")
  
  return(result)
}
