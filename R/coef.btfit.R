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

#' Extract coefficients of a 'btfit' object
#' @inheritParams btprob
#' @inheritParams summary.btfit
#' @return  If as_df = TRUE, a data frame a numeric vector of estimated coefficients, where the first column is the component the item is in, the second column in the item and the third column in the coefficient. If as_df = FALSE, then a numeric vector is returned if the model is fitted on the full dataset, or else a list of numeric vectors is returned, one for each fully connected component. Within each component, the items are arranged by estimate, in descending order.
#' @export
coef.btfit <- function(object, subset = NULL, ref = NULL,  as_df = FALSE, ...) {
    pi <- object$pi
    
    # check and get subset
    if (!is.null(subset)) {
      pi <- subset_by_pi(pi, subset)
    }
    
    # check the value of ref
    ref <- ref_check(ref, pi)
    
    # iterate over components
    result <- purrr::map(pi, coef_vec, ref = ref)
    
    # put into data frame, if requested
    if (as_df) {
      
      comp_names <- names(pi)

      result <- purrr::map(result, as_df_coef)
      
      reps <- purrr::map_int(result, nrow)
      
      result <- dplyr::bind_rows(result)        
        
      comps_for_df <- purrr::map2(comp_names, reps, ~rep(.x, each = .y))
      comps_for_df <- unlist(comps_for_df)
        
      result <- dplyr::mutate(result, component = comps_for_df)
      
      # hack to avoid CRAN note
      component <- NULL
      
      result <- dplyr::select(result, component, 1:2)
        
    }
    
    if (length(pi) == 1 & !as_df) {
      if(names(pi) == "full_dataset") {
        result <- unlist(result)
        names(result) <- names(pi[[1]])
      }
    }
    result
}
