#' Simulate data from a specified Bradley-Terry model
#'
#' @aliases simulate.btfit
#' @param pi a numeric vector, with all values finite and positive.
#' @param N  a symmetric, numeric matrix with dimensions the same as \code{length(pi)}.  The elements should
#' be numeric representations of non-negative integers.
#' @param nsim  a scalar integer, the number of datasets to be generated.
#' @param seed  an object specifying if and how the random number generator should be initialized (‘seeded’).  
#' For details see \code{\link{simulate}}.
#' @param result_class a character vector specifying whether the generated datasets should be of class 
#' "sparseMatrix" or of class "btdata".  The first match among those alternatives is used. 
#' @return a list of length \code{nsim} of simulated datasets, each dataset being a sparse matrix with the 
#' same dimensions as \code{N}.
#' @author David Firth
#' @export
simulate_BT <- function(pi, N, nsim = 1, seed = NULL, result_class = c("sparseMatrix", "btdata")){

  ## A simulate function that takes a vector pi and a matrix N as its arguments
  
  # set the seed, exactly as in simulate.lm 
  if (is.null(seed)) 
        RNGstate <- get(".Random.seed", envir = .GlobalEnv)
  else {
        R.seed <- get(".Random.seed", envir = .GlobalEnv)
        set.seed(seed)
        RNGstate <- structure(seed, kind = as.list(RNGkind()))
        on.exit(assign(".Random.seed", R.seed, envir = .GlobalEnv))
    }
    
  #  Check the arguments
  if ((!is.matrix(N)) && (!inherits(N, "sparseMatrix")) && !inherits(N, "denseMatrix")) stop("N is not a matrix")
  dN <- dim(N)
  if (diff(dN) != 0) stop("N is not a square matrix")
  if (any(N < 0)) stop("N has one or more negative elements")
  if (dN[1] != length(pi)) stop("the specified pi and N values are incompatible")
  if (!is.vector(pi)) stop("pi is not a vector")
  if (!is.numeric(pi)) stop("pi is not numeric")
  if (any (pi < 0)) stop("pi has one or more negative elements")
  result_class <- match.arg(result_class)
  if (!(result_class %in% c("sparseMatrix", "btdata"))) stop("invalid value of result.class")
    
  template <- N <- methods::as(N, "dgCMatrix")  ## template is the matrix container for a single sample
  
  N <- as.matrix(Matrix::summary(N))
  i <- N[, "i"]
  j <- N[, "j"]
  lower <- i > j               ## lower-triangle index vector
  upper <- order(i, j)[i > j]  ## upper-triangle indices in the right order
  N <- N[lower, 3]             ## all of the binomial totals
  number_of_binomials <- length(N) 
  probs <- btprob_vec(pi)
  probs <- suppressMessages(probs[lower.tri(probs)])
  
  # simulate the new datasets
  
  result <- matrix(as.numeric(stats::rbinom(nsim * number_of_binomials, N, probs)), 
                          number_of_binomials, nsim)
  result <- split(result, col(result))  ## to get a list, rather than a matrix
  names(result) <- paste("sim", seq_len(nsim), sep = "_")
                  
  fill_template <- function(contents) {
    ## Function to put the simulated binomial counts into the template matrix
      res <- template
      res@x[lower] <- contents
      res@x[upper] <- N - contents
      if (result_class == "sparseMatrix") return(res)
      else return(btdata(res))
  }  
  
  result <- lapply(result, fill_template)
  
  attr(result, "seed") <- RNGstate
  
  return(result)
}

#' @export
simulate.btfit <- function(object, nsim = 1, seed = NULL, result_class = c("sparseMatrix", "btdata"), ...){
    
    ##  The S3 method to apply to btfit model objects -- a wrapper for simulate_BT
    
    if (!inherits(object, "btfit")) stop("object should be a 'btfit' object")
    
    pi <- object$pi
    if (length(pi) != 1) stop("simulate.btfit cannot be used for multi-component data/models")
    pi <- pi[[1]]
    
    N <- object$N[[1]]
    
    result_class <- match.arg(result_class)
    
    simulate_BT(pi, N, nsim = nsim, seed = seed, result_class = result_class) 
    
}

