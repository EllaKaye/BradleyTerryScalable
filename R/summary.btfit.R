#' @export
summary_vec <- function(pi, N, ref = NULL){
    lambda <- coef_vec(pi, ref)
    vc <- vcov_vec(pi, N, ref)
    se <- sqrt(diag(vc))
    result <- data.frame(player = names(lambda), estimate = lambda, SE = se)
    rownames(result) <- names(lambda)
    class(result) <- c("summary.btfit", "data.frame")
    result
}

#' @export
summary.btfit <- function(object, ref = NULL, combine = FALSE, ...){
    
    if (!inherits(object, "btfit")) stop("object should be a 'btfit' object")
    
    pi <- object$pi
    N <- object$N
    
    if (is.list(pi)) {
        
      ## Restrict 'ref' value to NULL or 1 if there is >1 component
      if (!(is.null(ref)) && (ref != 1)) stop("The value of 'ref' should be 1 or NULL")
        
      result <- purrr::map2(pi, N, summary_vec, ref = ref)
      
      if (combine) {
        comp_num <- 1:length(pi)
        result <- purrr::map2(result, comp_num, ~ .x %>% dplyr::mutate(component = .y)) %>%
          dplyr::bind_rows()
      }
    }
    
    else result <- summary_vec(pi, N, ref = ref) 
    
    result
}
