#' @export
summary_vec <- function(pi, N, ref = NULL){
    lambda <- coef_vec(pi, ref)
    vc <- vcov_vec(pi, N, ref)
    se <- sqrt(diag(vc))
    result <- data.frame(estimate = lambda, SE = se)
    row.names(result) <- names(lambda)
    class(result) <- c("summary.btfit", "data.frame")
    result
}

#' @export
summary.btfit <- function(object, ref = NULL, combine_df = FALSE, ...){
    
    if (!inherits(object, "btfit")) stop("object should be a 'btfit' object")
    
    pi <- object$pi
    N <- object$N
    
    if (is.list(pi)) {
      result <- purrr::map2(pi, N, summary_vec, ref = ref)
      
      if (combine_df) {
        comp_num <- 1:length(pi)
        result <- purrr::map2(result, comp_num, ~ .x %>% dplyr::mutate(component = .y)) %>%
          dplyr::bind_rows()
      }
    }
    
    else result <- summary_vec(pi, N, ref = ref) 
    
    result
}
