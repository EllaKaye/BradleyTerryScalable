item_summary_vec <- function(pi, N, ref = NULL, SE = FALSE){
    lambda <- coef_vec(pi, ref)
    item <- names(lambda)
    
    result <- dplyr::data_frame(item = item, estimate = lambda)
    
    if (SE) {
      vc <- vcov_vec(pi, N, ref)
      se <- sqrt(diag(vc))
      result <- dplyr::mutate(result, SE = se)
    }
    

    #if (!is.null(names(lambda))) item <- names(lambda)
    #else item <- 1:length(lambda)
    #result <- dplyr::data_frame(item = item, estimate = lambda, SE = se)
    # result <- dplyr::arrange(result, dplyr::desc(estimate))
    #class(result) <- c("tbl_df", "tbl", "data.frame")
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
#' Note that the values given in the \code{estimate} column of the \code{item_summary} element are NOT the same as the values in \code{object$pi}. Rather, they are the \eqn{\lambda_i}, where \eqn{\lambda_i = \log{\pi_i}} (i.e. the coefficients as found by They are the coefficients, as found by \code{\link{coef.btfit}}.). By detault, these are normalised so that mean(\eqn{\lambda_i}) = 0. However, if \code{ref} is not equal to \code{NULL}, then the \eqn{\lambda_i} in the component in which \code{ref} appears are shifted to \eqn{\lambda_i - \lambda_{ref}}, for \eqn{i = 1, \dots, K_c}, where \eqn{K_c} is the number of items in the component in whick \code{ref} appears, and \eqn{\lambda_{ref}} is the estimate for the reference item.
#' 
#' @inheritParams btprob 
#' @param ref A reference item. Either a string with the item name, 1 or NULL. If NULL, then the coefficients are constrained such that their mean is zero. If an item name is given, the coefficient estimates are shifted so that the coefficient for the ref item is zero. If there is more than one component, the components that do not include the ref item will be treated as if ref = NULL. If ref = 1, then the first item of each component is made the reference item. See Details.
#' @param SE Logical. Whether to include the standard error of the estimate in the \code{item_summary} table. Default is \code{FALSE}. \strong{N.B. calculating the standard error can be very slow when the number of items is large}. See \code{\link{vcov.btfit}}.
#' @param ... other arguments
#' 
#' @return An S3 object of class \code{"summary.btfit"}. It is a list containing the following components:
#' \item{item_summary}{A \code{tibble} with columns for the item name, its coefficient, the standard error and the component it is in. Within each component, the items are arranged by estimate, in descending order. Note that the \code{estimate} is NOT the same as the values in \code{summary$pi}. See Details.}
#' \item{component_summary}{A \code{tibble} with a row for each component in the \code{btfit} object (named according to the original \code{btdata$components}, with the number of items in the component, the number of iterations the fitting algorithm ran for, and whether it converged.}
#' @seealso \code{\link{btfit}}, \code{\link{coef.btfit}}, \code{\link{vcov.btfit}}
#' @author Ella Kaye
#' @examples 
#' citations_btdata <- btdata(BradleyTerryScalable::citations)
#' fit1 <- btfit(citations_btdata, 1)
#' summary(fit1)
#' toy_df_4col <- codes_to_counts(BradleyTerryScalable::toy_data, c("W1", "W2", "D"))
#' toy_btdata <- btdata(toy_df_4col)
#' fit2a <- btfit(toy_btdata, 1)
#' summary(fit2a)
#' fit2b <- btfit(toy_btdata, 1.1)
#' summary(fit2b, SE = TRUE)
#' fit2c <- btfit(toy_btdata, 1)
#' summary(fit2c, subset = function(x) "Amy" %in% names(x))
#' summary(fit2c, subset = function(x) length(x) > 3, ref = "Amy")
#' @export
summary.btfit <- function(object, subset = NULL, ref = NULL, SE = FALSE, ...){
    
    if (!inherits(object, "btfit")) stop("object should be a 'btfit' object")
    
    call <- object$call
    pi <- object$pi
    N <- object$N
    iters <- object$iters
    converged <- object$converged

      
    ## subset
    if (!is.null(subset)) {
      
      pi <- subset_by_pi(pi, subset)
      new_pi_names <- names(pi)
      
      N <- N[new_pi_names]
      iters <- iters[new_pi_names]
      converged <- converged[new_pi_names]
    }
    
    ## check ref
    ref <- ref_check(ref, pi)
    
    # hack to avoid CRAN notes 
    component <- NULL
    
    # item summary
    summary_by_comp <- purrr::map2(pi, N, item_summary_vec, ref = ref, SE = SE)
    
    comp_names <- names(pi)
    
    reps <- purrr::map_int(summary_by_comp, nrow)
    
    item_summary_result <- dplyr::bind_rows(summary_by_comp)
    
    comps_for_df <- purrr::map2(comp_names, reps, ~rep(.x, each = .y))
    comps_for_df <- unlist(comps_for_df)
    
    item_summary_result <- dplyr::mutate(item_summary_result, component = comps_for_df)
    if (SE) item_summary_result <- dplyr::select(item_summary_result, component, item:SE)
    else item_summary_result <- dplyr::select(item_summary_result, component, item:estimate)
    
    
    # component summary
    component_summary_result <- purrr::pmap(list(pi, iters, converged), component_summary_vec)
    component_summary_result <- dplyr::bind_rows(component_summary_result)
    component_summary_result <- dplyr::mutate(component_summary_result, component = comp_names)
    component_summary_result <- dplyr::select(component_summary_result, component, num_items:converged)
    
    result <- list(call = call, item_summary = item_summary_result, component_summary = component_summary_result)
    
    #class(result) <- c("summary.btfit", "list")
    result
}
