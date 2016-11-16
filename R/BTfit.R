# Do we want an argument for start? Probably not - eigs is fastest.

BTfit <- function(W, a, b = NULL, components = NULL, ML_method = c("ILSR", "MM"), maxit = 100, epsilon = 1e-2, graph = FALSE) {

  ### Checks on the arguments
  if (!(is(W, "Matrix") | is.matrix(W) )) stop("W must be a square matrix")
  if (dim(W)[1] != dim(W)[2]) stop("W must be a square matrix")
  if (any(W < 0)) stop("All entries of W must by non-negative")
  if (a < 1) stop ("a must be >= 1")
  if ((a > 1) && (!is.null(b)) && (b < 0)) stop("b must be positive or NULL when a > 1")

  ### Make sure that matrix is of type dgCMatrix
  if (is.matrix(W)) W <- Matrix(W, sparse = TRUE)
  if (class(W) != "dgCMatrix") W <- as(W, "dgCMatrix")

  ### deal with dimnames and diagonals

  ### set up storage
  ### only want to deal with components of length greater that 1
  ### can we initialise list of correct length?
  pi_list <- vector("list", n) # where is the number of components

  return(W)
}
