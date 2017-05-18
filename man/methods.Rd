\name{summary.btfit}
\alias{coef.btfit}
\alias{vcov.btfit}
\alias{summary.btfit}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Standard methods for model objects of class 'btfit'
}
\description{
These are S3 methods for the generic \code{link(coef}}, \code{\link{vcov}} and \code{\link{summary}} functions.  
}
\usage{
coef.btfit(object, ref = NULL, ...)
vcov.btfit(object, ref = NULL, ...)
summary.btfit(object, ref = NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
A Bradley-Terry model object produced by the \code{\link{btfit}} function.
}
  \item{ref}{
Either \code{NULL} (the default), or the length-1 index to specify a particular element of the coefficient vector as the reference value.  If \code{NULL} is used, the constraint applied is that the mean of the coefficients is zero. If \code{object$pi} is a list, i.e. with estimates for multiple components, then \code{ref} must be either \code{NULL} or \code{1}.  
}
  \item{\dots}{
Other possible arguments.
}
}
\value{
The \code{\link{coef}} method results in a numeric vector of estimated coefficients, or a list of such vectors if \code{object} has more than one component.

The \code{\link{vcov}} method results in a square numeric matrix, which is a non-full-rank variance-covariance matrix for the estimates in \code{coef(object, ref = ref)}; or a list of such matrices if \code{object} has more than one component.

The \code{\link{summary}} method results in a summary data frame of the estimates and standard errors, or a list of such data frames if \code{object} has more than one component.
}
