\name{vcov.btfit}
\alias{vcov.btfit}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Variance-covariance method for objects of class 'btfit'
}
\description{
This is a method for the generic \code{\link{vcov}} function.  It computes a non-full rank variance-covariance matrix for the estimates of a set of contrasts.
}
\usage{
vcov.btfit(object, ref = NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{
A Bradley-Terry model object produced by the \code{\link{btfit}} function.
}
  \item{ref}{
Either \code{NULL} (the default), or the length-1 index to specify an element of the vector \code{object $ pi} as the reference value.  If \code{NULL} is used, the constraint used is the default for \code{\link{btfit}}, namely that  \code{sum(object $ pi)} is set equal to 1.  
}
  \item{\dots}{
Other possible arguments to the \code{\link{vcov}} function.
}
}
\value{
A square numeric matrix, which is a non-full-rank variance-covariance matrix for the estimates of a set of specified contrasts among values of \code{log(object $ pi)}.
}




