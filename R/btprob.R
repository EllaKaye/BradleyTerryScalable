


btprob <- function(pi) {

  if(!is.vector(pi)) stop("pi should be a numeric vector or a list of numeric vectors")
  if(!is.list(pi) & !is.numeric(pi)) stop("pi should be a numeric vector or a list of numeric vectors")
  if(is.list(pi) & (sum(sapply(pi, is.numeric)) != length(pi))) stop("pi should be a numeric vector or a list of numeric vectors")


  if (is.list(pi)) {
    pi_list_names <- lapply(pi, names)
    unnamed <- lapply(pi, btprob_vec)
    out <- mapply(function(x,y) {
      dimnames(x) <- list(y, y)
      return(x)
    }, unnamed, pi_list_names)
  }

  else {
    pi_names <- names(pi)
    out <- btprob_vec(pi)
    dimnames(out) <- list(pi_names, pi_names)
  }

  out
}


