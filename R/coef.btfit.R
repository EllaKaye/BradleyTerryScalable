#' @export
coef_vec <- function(pi, ref = NULL, ...){
  coefs <- log(pi)
  ncoefs <- length(coefs)
  if (is.null(ref)) return(coefs - mean(coefs))
  object_names <- names(coefs)
  if (ref %in% object_names) return(coefs - coefs[ref])
  else if (ref == 1) return(coefs - coefs[1])
  else return(coefs - mean(coefs))
  #if (ref %in% object_names) ref <- which(object_names == ref)
  #if (ref %in% seq(coefs)) {
  #  return(coefs - coefs[ref])
  #} else stop("Invalid value for the 'ref' argument")
}

#' @export
coef.btfit <- function(object, ref = NULL, ...) {
    pi <- object$pi
    
    # check the value of ref
    ref <- ref_check(ref, pi)
    
    # iterate over components
    result <- purrr::map(pi, coef_vec, ref = ref)
    
    if (length(pi) == 1) {
      if(names(pi) == "full_dataset") {
        result <- unlist(result)
        names(result) <- names(pi[[1]])
      }
    }
    result
}
