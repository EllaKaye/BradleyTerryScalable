#' @export
summary.btfit <- function(object, ref = NULL, ...){
  
  if (!inherits(object, "btfit")) stop("object should be a 'btfit' object")
  
    pvec <- object$pi
    k <- length(pvec)
    lambda <- log(pvec)
    if (!is.null(ref)) {  ## The specified constraint is that one of the log-ability parameters is zero
        if (ref %in% names(pvec)) ref <- which(names(pvec) == ref)
        if (ref %in% 1:k) {
            lambda <- lambda - lambda[ref]
        } else stop("Invalid value for the 'ref' argument")
    }
    vc <- vcov(object, ref = ref)
    se <- sqrt(diag(vc))
    result <- data.frame(estimate = lambda, SE = se)
    row.names(result) <- names(pvec)
    class(result) <- c("summary.btfit", "data.frame")
    return(result)
}
