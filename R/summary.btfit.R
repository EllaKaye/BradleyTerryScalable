item_summary_vec <- function(pi, N, ref = NULL){
    lambda <- coef_vec(pi, ref)
    vc <- vcov_vec(pi, N, ref)
    se <- sqrt(diag(vc))
    if (!is.null(names(lambda))) item <- names(lambda)
    else item <- 1:length(lambda)
    result <- dplyr::data_frame(item = item, estimate = lambda, SE = se)
    result <- dplyr::arrange(result, dplyr::desc(estimate))
    class(result) <- c("tbl_df", "tbl", "data.frame")
    result
}

component_summary_vec <- function(pi, iters, converged) {
  
  num_items <- length(pi)
  
  result <- dplyr::data_frame(num_items = num_items, iters = iters, converged = converged)
  
  class(result) <- c("tbl_df", "tbl", "data.frame")
  result  
}

#' Summarizing Bradley-Terry Fits
#' 
#' \code{summary} method for class "btfit"
#' 
#' @inheritParams btprob 
#' @param ref the reference item, either a string with the item name, 1 or NULL. If NULL, then the coefficients are constrained such that their mean is zero. If an item name is given, they are shifted so that the coefficient for the ref item is zero. If there is more than one component, the components that do not include the ref item will be treated as if ref = NULL. If ref = 1, then the first item of each component is made the reference item.
#' @param ... other arguments
#' 
#' @return An S3 object of class \code{"summary.btfit"}. It is a list containing the following components:
#' \item{item_summary}{A tibble with columns for the item name, its coefficient, the standard error and the component it is in.}
#' \item{component_summary}{A tibble with a row for each component in the \code{btfit} object (named according to the original \code{btdata$components}, with the number of items in the component, the number of iterations the fitting algorithm ran for, and whether it converged.}
#' 
#' @export
summary.btfit <- function(object, ref = NULL, subset = NULL, ...){
    
    if (!inherits(object, "btfit")) stop("object should be a 'btfit' object")
    
    call <- object$call
    pi <- object$pi
    N <- object$N
    iters <- object$iters
    converged <- object$converged

        
    ## check ref
    ref <- ref_check(ref, pi)
    
    ## subset
    if (!is.null(subset)) {
      if (!is.character(subset)) stop("subset should be a character vector")
      if(!all(subset %in% names(pi))) stop("not all elements of subset are names of components")
      
      pi <- pi[subset]
      N <- N[subset]
      iters <- iters[subset]
      converged <- converged[subset]
    }
    
    # hack to avoid CRAN notes 
    component <- NULL
    
    # item summary
    summary_by_comp <- purrr::map2(pi, N, item_summary_vec, ref = ref)
    
    comp_names <- names(pi)
    
    reps <- purrr::map_int(summary_by_comp, nrow)
    
    item_summary_result <- dplyr::bind_rows(summary_by_comp)
    
    comps_for_df <- purrr::map2(comp_names, reps, ~rep(.x, each = .y))
    comps_for_df <- unlist(comps_for_df)
    
    item_summary_result <- dplyr::mutate(item_summary_result, component = comps_for_df)
    
    
    # component summary
    component_summary_result <- purrr::pmap(list(pi, iters, converged), component_summary_vec)
    component_summary_result <- dplyr::bind_rows(component_summary_result)
    component_summary_result <- dplyr::mutate(component_summary_result, component = comp_names)
    component_summary_result <- dplyr::select(component_summary_result, component, num_items:converged)
    
    result <- list(call = call, item_summary = item_summary_result, component_summary = component_summary_result)
    
    #class(result) <- c("summary.btfit", "list")
    result
}
