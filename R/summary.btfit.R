#' @export
summary.btfit <- function(object, ref = NULL, ...){
  
  if (!inherits(object, "btfit")) stop("object should be a 'btfit' object")

    lambda <- object$lambda
    K <- length(lambda)
    # pi <- exp(lambda)

    if (!is.null(ref)) {  ## The specified constraint is that one of the log-ability parameters is zero
        if (ref %in% names(lambda)) ref <- which(names(lambda) == ref)
        if (ref %in% 1:K) {
            lambda <- lambda - lambda[ref]
        } else stop("Invalid value for the 'ref' argument")
    }
    vc <- vcov(object, ref = ref)
    se <- sqrt(diag(vc))
    result <- data.frame(estimate = lambda, SE = se)
    row.names(result) <- names(lambda)
    class(result) <- c("summary.btfit", "data.frame")
    result
}
