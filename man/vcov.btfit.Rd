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

\author{
David Firth
}
\examples{
library(BradleyTerry2)
data(citations)
citations_model <- btfit(citations, 1, epsilon = 1e-8)
##  First get variance-covariance matrix in the default parameterization
v1 <- vcov(citations_model)
##  Now two different ways to specify comparison with the first journal:
v2a <- vcov(citations_model, ref = 1)
v2b <- vcov(citations_model, ref = "Biometrika")
##
##  Compare v2a with v2b (they are identical):
identical(v2a, v2b)
##  Compare v2a/b with corresponding results from the BradleyTerry2 package:
citeModel <-  BTm(cbind(win1, win2), journal1, journal2, data = citations.sf)
vcov(citeModel) - v2a[-1, -1]   ## essentially zero
##
## Obtain v2a or v2b from v1, by specifying a contrast matrix:
cmat <- rbind(0, cbind(-1, diag(3)))
rownames(cmat) <- colnames(cmat) <- rownames(v1)
all.equal(cmat %*% v1 %*% t(cmat), v2a)           ## TRUE
}
\keyword{ models }

