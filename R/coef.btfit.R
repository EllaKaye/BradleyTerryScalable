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

as_df_coef <- function(vec) {
  dplyr::tibble(item = names(vec), coef = unname(vec))
}

#' @export
coef.btfit <- function(object, ref = NULL, subset = NULL, as_df = FALSE, ...) {
    pi <- object$pi
    
    # check and get subset
    if (!is.null(subset)) {
      if (!is.character(subset)) stop("subset should be a character vector")
      if(!all(subset %in% names(pi))) stop("not all elements of subset are names of components")
      pi <- pi[subset]
    }
    
    # check the value of ref
    ref <- ref_check(ref, pi)
    
    # iterate over components
    result <- purrr::map(pi, coef_vec, ref = ref)
    
    # put into data frame, if requested
    if (as_df) {
      
      comp_names <- names(pi)

      result <- purrr::map(result, as_df_coef) %>%
        purrr::map2(comp_names, ~ .x %>% dplyr::mutate(component = .y)) %>%
        dplyr::bind_rows()        
    }
    
    if (length(pi) == 1 & !as_df) {
      if(names(pi) == "full_dataset") {
        result <- unlist(result)
        names(result) <- names(pi[[1]])
      }
    }
    result
}
