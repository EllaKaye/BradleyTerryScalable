#' @export
summary_vec <- function(pi, N, ref = NULL){
    lambda <- coef_vec(pi, ref)
    vc <- vcov_vec(pi, N, ref)
    se <- sqrt(diag(vc))
    if (!is.null(names(lambda))) item <- names(lambda)
    else item <- 1:length(lambda)
    result <- dplyr::data_frame(item = item, estimate = lambda, SE = se)
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

#' @export
summary.btfit <- function(object, ref = NULL, ...){
    
    if (!inherits(object, "btfit")) stop("object should be a 'btfit' object")
    
    pi <- object$pi
    N <- object$N
    iters <- object$iters
    converged <- object$converged

        
    ## Restrict 'ref' value to NULL or 1 if there is >1 component
    if (!(is.null(ref)) && (ref != 1)) stop("The value of 'ref' should be 1 or NULL")
        
    summary_by_comp <- purrr::map2(pi, N, summary_vec, ref = ref)
      
    comp_names <- names(pi)
    summary_result <- purrr::map2(summary_by_comp, comp_names, ~ .x %>% dplyr::mutate(component = .y)) %>%
        dplyr::bind_rows()
    
    
    component_summary_result <- purrr::pmap(list(pi, iters, converged), component_summary_vec) %>%
      dplyr::bind_rows() %>% 
      dplyr::mutate(component = comp_names) %>%
      dplyr::select(component, num_items:converged)

    
    result <- list(summary_result = summary_result, component_summary_result = component_summary_result)
    
    class(result) <- c("summary.btfit", "list")
    result
}
