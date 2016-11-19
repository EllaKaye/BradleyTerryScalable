btprob <- function(pi) {

  if(!is.vector(pi)) stop("pi should be a numeric vector or a list of numeric vectors")
  if(!is.list(pi) & !is.numeric(pi)) stop("pi should be a numeric vector or a list of numeric vectors")
  if(is.list(pi) & (sum(sapply(pi, is.numeric)) != length(pi))) stop("pi should be a numeric vector or a list of numeric vectors")

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
