#' @export
coef_vec <- function(pi, ref = NULL, ...){
  coefs <- log(pi)
  ncoefs <- length(coefs)
  if (is.null(ref)) return(coefs - mean(coefs))
  object_names <- names(coefs)
  if (ref %in% object_names) return(coefs - coefs[ref])
  else return(coefs - mean(coefs))
  #if (ref %in% object_names) ref <- which(object_names == ref)
  #if (ref %in% seq(coefs)) {
  #  return(coefs - coefs[ref])
  #} else stop("Invalid value for the 'ref' argument")
}

#' @export
coef.btfit <- function(object, ref = NULL, ...) {
    pi <- object$pi
        
    ## Restrict 'ref' value to NULL or 1 if there is >1 component
    #if (!(is.null(ref)) && (ref != 1)) stop("The value of 'ref' should be 1 or NULL")
    if (!(is.null(ref))) {
      names <- purrr::map(pi, ~ names(.x)) %>% unlist()
      if (!(ref %in% names)) {
        ref <- NULL
        warning("The value of ref is not an item name. Using ref = NULL instead")
      }
    }
          
    result <- purrr::map(pi, coef_vec, ref = ref)
    if (length(pi) == 1) {
      result <- unlist(result)
      names(result) <- names(pi[[1]])
    }
    result
}
