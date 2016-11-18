btfit <- function(W, a, b = NULL, components = NULL, ML_method = c("ILSR", "MM"), maxit = 100, epsilon = 1e-2, graph = FALSE) {

  ### Checks on the arguments
  if (!(is(W, "Matrix") | is.matrix(W) )) stop("W must be a square matrix")
  if (dim(W)[1] != dim(W)[2]) stop("W must be a square matrix")
  if (any(W < 0)) stop("All entries of W must by non-negative")
  if (a < 1) stop ("a must be >= 1")
  if ((a > 1) && (!is.null(b)) && (b < 0)) stop("b must be positive or NULL when a > 1")
  if (!(is.null(rownames(W)) & is.null(colnames(W)))) {
    if(sum(rownames(W) != colnames(W)) > 0) stop("rownames and colnames of W should be the same")
  }

  ### check if provided components appear to match W
  ### NB, this only covers checking the elements of the components match the row/colnames of W,
  ### not whether they are actually the correct components
  if (a == 1 & !is.null(components)) {

    if(!is.list(components)) stop("When a = 1, components should be NULL or a list of components (preferably the output of connected_components(W))")

    comp_elements <- unlist(components, use.names = FALSE)
    if(length(comp_elements) != nrow(W)) stop("There are a different number of elements in the components than there are rows/columns in W")

    if (!is.null(rownames(W))) {
      if (!setequal(comp_elements, rownames(W))) stop("Elements in components do not match the row/colnames of W")
    }
  }


  ### Make sure that matrix is of type dgCMatrix
  if (is.matrix(W)) W <- Matrix(W, sparse = TRUE)
  if (class(W) != "dgCMatrix") W <- as(W, "dgCMatrix")

  ### decide whether to return graph
  return_graph <- FALSE
  if (a == 1 & is.null(components) & graph) return_graph <- TRUE

  ### calculate components, if necessary
  if (a == 1 & is.null(components)) {
    g <- igraph::graph.adjacency(W, weighted = TRUE, diag = FALSE)
    components <- igraph::groups(igraph::components(g, mode = "strong"))
  }

  ### remove components of length 1
  components <- Filter(function(x) length(x) > 1, components)

  ### get necessary dimensions and set up storage
  n <- length(components)
  K <- nrow(W)

  ### Get MLE per component if a = 1
  if (a == 1) {

    ML_method <- match.arg(ML_method)

    # When n == 1, just use all of W
    if (n == 1) {
      if (ML_method == "ILSR") {
        if (K == 2) fit <- BT_EM(W, a = 1, b = 0, maxit = maxit, epsilon = epsilon)
        else fit <- ILSR(W, maxit = maxit, epsilon = epsilon)
      }
      if (ML_method == "MM") fit <- BT_EM(W, a = 1, b = 0, maxit = maxit, epsilon = epsilon)

      pi <- base::as.vector(fit$pi)
      names(pi) <- rownames(W)
      iters <- fit$iters
      converged <- fit$converged
    } # end n == 1 if

    # When n > 1, return by component
    else {
      pi <- vector("list", n) #  list to hold vectors of pi
      iters <- numeric(n) # vector to hold number of iterations per component
      converged <- logical(n)

      for (k in 1:n) {
        compk <- components[[k]] # the players in component k
        Wsub <- W[compk, compk] # wins matrix for players in component k

        if (ML_method == "ILSR") {

          if (length(compk) == 2) fit <- BT_EM(Wsub, a = 1, b = 0, maxit = maxit, epsilon = epsilon)
          else fit <- ILSR(Wsub, maxit = maxit, epsilon = epsilon)
        }
        if (ML_method == "MM") fit <- BT_EM(Wsub, a = 1, b = 0, maxit = maxit, epsilon = epsilon)

        pi[[k]] <- base::as.vector(fit$pi)
        names(pi[[k]]) <- compk
        iters[k] <- fit$iters
        converged[k] <- fit$converged
      }

      # check for convergence problems and provide warning
      if (sum(converged) != n) warning("The algorithm did not converge in at least one component. See the 'converged' element of the output for which.")

    } # end n > 1

  } # end a == 1 if

  ### Get penalised if a > 1
  else {

    if (is.null(b)) b <- a * K - 1

    ### penalised by component, if components are provided
    if (!is.null(components)) {

      pi <- vector("list", n) #  list to hold vectors of pi
      iters <- numeric(n) # vector to hold number of iterations per component
      converged <- logical(n)

      for (k in 1:n) {
        compk <- components[[k]] # the players in component k
        Wsub <- W[compk, compk] # wins matrix for players in component k

        fit <- BT_EM(Wsub, a = a, b = b, maxit = maxit, epsilon = epsilon)

        pi[[k]] <- base::as.vector(fit$pi)
        names(pi[[k]]) <- compk
        iters[k] <- fit$iters
        converged[k] <- fit$converged
      } # end loop over components

      # check for convergence problems and provide warning
      if (sum(converged) != n) warning("The algorithm did not converge in at least one component. See the 'converged' element of the output for which.")
    } # end fit by component

    ### EM-algorithm on full W, if no components are provided
    else {

      fit <- BT_EM(W, a = a, b = b, maxit = maxit, epsilon = epsilon)
      pi <- base::as.vector(fit$pi)
      iters <- fit$iters
      converged <- fit$converged

    } # end EM on full W

  } ## end a > 1 if

  result <- list(pi = pi, iters = iters, converged = converged)
  if (return_graph) result$graph <- g

  return(result)
}


