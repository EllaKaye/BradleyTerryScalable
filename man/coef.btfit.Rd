% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/coef.btfit.R
\name{coef.btfit}
\alias{coef.btfit}
\title{Extract coefficients of a 'btfit' object}
\usage{
\method{coef}{btfit}(object, subset = NULL, ref = NULL, as_df = FALSE, ...)
}
\arguments{
\item{object}{An object of class "btfit", typically the result \code{ob} of \code{ob <- btfit(..)}. See \code{\link{btfit}}.}

\item{subset}{A condition for selecting one or more subsets of the components. This can either be a character vector of names of the components (i.e. a subset of \code{names(object$pi)}), a single predicate function (that takes a vector of \code{object$pi} as its argument), or a logical vector of the same length as the number of components, (i.e. \code{length(object$pi)}).}

\item{ref}{A reference item. Either a string with the item name, or the number 1, or NULL. If NULL, then the coefficients are constrained such that their mean is zero. If an item name is given, the coefficient estimates are shifted so that the coefficient for the ref item is zero. If there is more than one component, the components that do not include the ref item will be treated as if ref = NULL. If ref = 1, then the first item of each component is made the reference item.}

\item{as_df}{Logical scalar, determining class of output. If TRUE, the function returns a data frame. If FALSE (the default), the function returns a named vector (or list of such vectors).}

\item{...}{other arguments}
}
\value{
If as_df = TRUE, a data frame a numeric vector of estimated coefficients, where the first column is the component the item is in, the second column in the item and the third column in the coefficient. If as_df = FALSE, then a numeric vector is returned if the model is fitted on the full dataset, or else a list of numeric vectors is returned, one for each fully connected component. Within each component, the items are arranged by estimate, in descending order.
}
\description{
\code{coef} method for class "btfit"
}
\details{
Note that the values given in the \code{estimate} column of the \code{item_summary} element are NOT the same as the values in \code{object$pi}. Rather, they are the \eqn{\lambda_i}, where \eqn{\lambda_i = \log{\pi_i}}. By default, these are normalised so that mean(\eqn{\lambda_i}) = 0. However, if \code{ref} is not equal to \code{NULL}, then the \eqn{\lambda_i} in the component in which \code{ref} appears are shifted to \eqn{\lambda_i - \lambda_{ref}}, for \eqn{i = 1, \dots, K_c}, where \eqn{K_c} is the number of items in the component in which \code{ref} appears, and \eqn{\lambda_{ref}} is the estimate for the reference item.
}
\examples{
citations_btdata <- btdata(BradleyTerryScalable::citations)
fit1 <- btfit(citations_btdata, 1)
coef(fit1)
toy_df_4col <- codes_to_counts(BradleyTerryScalable::toy_data, c("W1", "W2", "D"))
toy_btdata <- btdata(toy_df_4col)
fit2a <- btfit(toy_btdata, 1)
coef(fit2a)
coef(fit2a, subset = function(x) length(x) > 3, as_df = TRUE)
coef(fit2a, subset = function(x) "Amy" \%in\% names(x))
coef(fit2a, as_df = TRUE)
fit2b <- btfit(toy_btdata, 1.1)
coef(fit2b)
coef(fit2b, ref = "Cyd")
}
\author{
Ella Kaye
}
