vcov_vec <- function(pi, N, ref = NULL) {
  K <- length(pi)
  object_names <- names(pi)
  wmat <- fitted_vec(pi, N)
  pmat <- btprob_vec(pi)
  result <- - wmat * Matrix::t(pmat)
  diag(result) <- - Matrix::rowSums(result)

  cmat <- stats::contr.sum(K, sparse = TRUE)
  result <- Matrix::chol2inv(Matrix::chol(Matrix::crossprod(cmat, result) %*% cmat))
  result <- cmat %*% Matrix::tcrossprod(result, cmat)

  ##  That's the essence of the calculation all done.  We have computed the
  ##  (singular) variance-covariance matrix of all contrasts with the mean.
  ##
  ##
  ##  The rest is all about potentially presenting the result as the vcov matrix
  ##  for contrasts with a specified reference item, instead.
  ##

  if (!is.null(ref)) {
      if (ref %in% object_names) ref <- which(names(pi) == ref)
      cmat <- Matrix::Diagonal(K)
      cmat[ref,] <- -1
      cmat[,ref] <-  0
      result <- Matrix::crossprod(cmat, result) %*% cmat
  }

  rownames(result) <- colnames(result) <- object_names
  return(result)
}

#' Calculate variance-covariance matrix for a btfit object
#' 
#' \code{vcov} method for class "btfit"
#' 
#' \strong{N.B. this can be slow when there are a large number of items in any component.}
#' 
#' @inheritParams btprob
#' @inheritParams summary.btfit
#' @return  A square numeric matrix, which is a non-full-rank variance-covariance matrix for the estimates in \code{coef(object, subset = subset, ref = ref)}; or a list of such matrices if \code{object} has more than one component. The rows and columns of the matrix (or matrices) are arranged in the same order as the \code{object$pi} vector(s).
#' -#' @author David Firth, Ella Kaye
#' @seealso \code{\link{btfit}}, \code{\link{coef.btfit}}, \code{\link{summary.btfit}}
#' @examples 
#' citations_btdata <- btdata(BradleyTerryScalable::citations)
#' #' fit1 <- btfit(citations_btdata, 1)
#' #' vcov(fit1)
#' toy_df_4col <- codes_to_counts(BradleyTerryScalable::toy_data, c("W1", "W2", "D"))
#' toy_btdata <- btdata(toy_df_4col)
#' fit2a <- btfit(toy_btdata, 1)
#' vcov(fit2a)
#' vcov(fit2a, subset = function(x) length(x) > 3)
#' vcov(fit2a, subset = function(x) "Cyd" %in% names(x))
#' fit2b <- btfit(toy_btdata, 1.1)
#' vcov(fit2b, ref = "Cyd")
#' 
#' @export
vcov.btfit <- function(object, subset = NULL, ref = NULL, ...){

  if (!inherits(object, "btfit")) stop("object should be a 'btfit' object")

    pi <- object$pi
    N <- object$N
    names_dimnames <- object$names_dimnames
    names_dimnames_list <- list(names_dimnames)

    # check and get subset
    if (!is.null(subset)) {

      pi <- subset_by_pi(pi, subset)
      new_pi_names <- names(pi)

      N <- N[new_pi_names]
    }

    # check the value of ref
    ref <- ref_check(ref, pi)

    # iterate over components
    result <- purrr::map2(pi, N, vcov_vec, ref = ref)
    result <- purrr::map2(result, names_dimnames_list, name_dimnames_function)

    if (length(pi) == 1) {
      if(names(pi) == "full_dataset") {
        result <- result[[1]]
      }
    }

    result
}
