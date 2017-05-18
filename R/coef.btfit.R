#' @export
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
    pi <- object$pi
    
    if (is.list(pi)) result <- purrr::map(pi, coef_vec, ref = ref)
    
    else result <- coef_vec(pi, ref = ref)
    
    result
}
