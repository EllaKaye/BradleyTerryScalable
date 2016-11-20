btprob <- function(pi) {

  if(!is.vector(pi)) stop("pi should be a numeric vector or a list of numeric vectors")
  if(!is.list(pi) & !is.numeric(pi)) stop("pi should be a numeric vector or a list of numeric vectors")
  if(is.list(pi) & (sum(sapply(pi, is.numeric)) != length(pi))) stop("pi should be a numeric vector or a list of numeric vectors")


  if (is.list(pi)) {
    lapply(pi, btprob_vec)
  }

  else (btprob_vec(pi))
}

