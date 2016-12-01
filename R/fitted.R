#' @export
fitted.btfit <- function(btfit, as_df = FALSE){
  if (!inherits(btfit, "btfit")) stop("Argument should be a 'btfit' object")

  pi <- btfit$pi
  N <- btfit$N
  diagonal <- btfit$diagonal

  if (is.list(pi)) {
    my_fitted <- function(pi, N) {
      p <- btprob_vec(pi)
      p * N
    }

    out <- Map(my_fitted, pi, N)

    my_diag <- function(x,y) {
      diag(x) <- y
      return(x)
    }

    out <- Map(my_diag, out, diagonal)

    if (as_df) {
      out <- lapply(out, as_df)
    }

  }

  else {
    p <- btprob_vec(pi)
    out <- p * N
    diag(out) <- diagonal

    if (as_df) out <- as_df(out)
  }

  return(out)

}
