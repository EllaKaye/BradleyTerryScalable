#' @export
vcov_vec <- function(pi, N, ref = NULL) {
  K <- length(pi)
  wmat <- fitted_vec(pi, N)
  pmat <- btprob_vec(pi)
  result <- wmat * Matrix::t(pmat)
  diag(result) <- Matrix::rowSums(wmat * pmat) + Matrix:::diag(wmat)
  result <- MASS::ginv(as.matrix(((diag(Matrix::rowSums(wmat)) - result))))
  
  ##  That's the essence of the calculation all done.  We have computed the Moore-Penrose generalized
  ##  inverse for the over-parameterized set of log-ability parameters.
  ##
  ##  The rest is all about presenting the result as the /actual/ vcov matrix for a specified
  ##  set of contrass (or equivalently a specified constraint on the parameters).
  if (is.null(ref)) {
    ##  Default contrasts are with mean(log(pi)):
    theContrasts <- diag(K) - matrix(1/K, K, K, byrow = TRUE)
  } 
  
  else {  ## The specified constraint is that one of the log-ability parameters is zero
    object_names <- names(pi)
    theContrasts <- diag(K)
    if (ref %in% object_names) {
      ref <- which(names(pi) == ref)
      theContrasts[, ref] <- theContrasts[, ref] - 1
    }
    else if (ref == 1) theContrasts[, ref] <- theContrasts[, ref] - 1
    else theContrasts <- diag(K) - matrix(1/K, K, K, byrow = TRUE)
  }
  result <- theContrasts %*% tcrossprod(result, theContrasts)
  rownames(result) <- colnames(result) <- names(pi)
  result
}

#' @export
vcov.btfit <- function(object, ref = NULL, ...){
  
  if (!inherits(object, "btfit")) stop("object should be a 'btfit' object")
  
    pi <- object$pi
    N <- object$N
    
    # check and get subset
    if (!is.null(subset)) {
      if (!is.character(subset)) stop("subset should be a character vector")
      if(!all(subset %in% names(pi))) stop("not all elements of subset are names of components")
      pi <- pi[subset]
      N <- N[subset]
    }
    
    # check the value of ref
    ref <- ref_check(ref, pi)
    
    # iterate over components
    result <- purrr::map2(pi, N, vcov_vec, ref = ref)
    
    if (length(pi) == 1) {
      if(names(pi) == "full_dataset") {
        result <- result[[1]]
      }
    }

    result
}
