btprob <- function(pi) {

  #if(!(!is.vector(pi) & !is.list(pi))) stop("1. pi should be a numeric vector or a list of numeric vectors")
  #if(is.list(pi) & (sum(sapply(pi, is.numeric)) != length(pi))) stop("3. pi should be a numeric vector or a list of numeric vectors")
  #if(is.vector(pi) & !is.numeric(pi)) stop("2. pi should be a numeric vector or a list of numeric vectors")

  btprob_vec <- function(pi) {
    mat <- pi * 1/outer(pi, pi, FUN = "+")
    diag(mat) <- 0
    return(mat)
  }

  if (is.list(pi)) {
    lapply(pi, btprob_vec)
  }

  else (btprob_vec(pi))
}
