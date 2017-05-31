#' @export
item_summary_vec <- function(pi, N, ref = NULL){
    lambda <- coef_vec(pi, ref)
    vc <- vcov_vec(pi, N, ref)
    se <- sqrt(diag(vc))
    if (!is.null(names(lambda))) item <- names(lambda)
    else item <- 1:length(lambda)
    result <- dplyr::data_frame(item = item, estimate = lambda, SE = se) %>%
      arrange(desc(estimate))
    class(result) <- c("tbl_df", "tbl", "data.frame")
    result
}

#' @export
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
#' 
#' @return An S3 object of class \code{"summary.btfit"}. It is a list containing the following components:
#' \item{item_summary}{A tibble with columns for the item name, its coefficient, the standard error and the component it is in.}
#' \item{component_summary}{A tibble with a row for each component in the \code{btfit} object (named according to the original \code{btdata$components}, with the number of items in the component, the number of iterations the fitting algorithm ran for, and whether it converged.}
#' 
#' @export
summary.btfit <- function(object, ref = NULL, ...){
    
    if (!inherits(object, "btfit")) stop("object should be a 'btfit' object")
    
    call <- object$call
    pi <- object$pi
    N <- object$N
    iters <- object$iters
    converged <- object$converged

        
    ## check ref
    ref <- ref_check(ref, pi)
        
    summary_by_comp <- purrr::map2(pi, N, item_summary_vec, ref = ref)
      
    comp_names <- names(pi)
    summary_result <- purrr::map2(summary_by_comp, comp_names, ~ .x %>% dplyr::mutate(component = .y)) %>%
        dplyr::bind_rows()
    
    
    component_summary_result <- purrr::pmap(list(pi, iters, converged), component_summary_vec) %>%
      dplyr::bind_rows() %>% 
      dplyr::mutate(component = comp_names) %>%
      dplyr::select(component, num_items:converged)
    
    result <- list(call = call, item_summary = summary_result, component_summary = component_summary_result)
    
    #class(result) <- c("summary.btfit", "list")
    result
}
