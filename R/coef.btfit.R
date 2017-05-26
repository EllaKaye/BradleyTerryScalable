#' @export
coef_vec <- function(pi, ref = NULL, ...){
  coefs <- log(pi)
  ncoefs <- length(coefs)
  if (is.null(ref)) return(coefs - mean(coefs))
  object_names <- names(coefs)
  if (ref %in% object_names) return(coefs - coefs[ref])
  if (ref == 1) return(coefs - coefs[1])
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
    if (!is.null(ref)) {
      if (is.character(ref)) {
        if (length(ref) != 1) stop("'ref' should be the name of an item, 1, or NULL")
        names <- purrr::map(pi, ~ names(.x)) %>% unlist()
        if (!(ref %in% names)) {
          ref <- NULL
          warning("The value of 'ref' is not an item name. Using ref = NULL instead")
        }
        
      }
      else if (is.numeric(ref)) {
        if (length(ref) != 1) stop("'ref' should be the name of an item, 1, or NULL")
        if (ref != 1) stop("'ref' should be the name of an item, 1, or NULL")
      }
      else stop("invalid value of ref")
    }
    
    result <- purrr::map(pi, coef_vec, ref = ref)
    if (length(pi) == 1) {
      if(names(pi) == "full_dataset") {
        result <- unlist(result)
        names(result) <- names(pi[[1]])
      }
    }
    result
}
