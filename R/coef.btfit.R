coef_vec <- function(pi, ref = NULL, ...){
  coefs <- log(pi)
  ncoefs <- length(coefs)
  if (is.null(ref)) return(coefs - mean(coefs))
  object_names <- names(coefs)
  if (ref %in% object_names) ref <- which(object_names == ref)
  if (ref %in% seq(coefs)) {
    return(coefs - coefs[ref])
  } else stop("Invalid value for the 'ref' argument")
}

#' @export
coef.btfit <- function(object, ref = NULL, ...){
 ##  This still needs the purrr magic!  For now just do the single component case:
    pi <- object$pi
    return(coef_vec(pi, ref = ref))
}
