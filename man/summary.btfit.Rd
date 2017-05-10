\name{summary.btfit}
\alias{summary.btfit}
\title{
Summary of a 'btfit' model object
}
\description{
This summary function provides a data frame of parameter estimates and associated standard errors, for a specified model parameterization.
}
\usage{
summary.btfit(object, ref = NULL, ...)
}
\arguments{
  \item{object}{
A Bradley-Terry model object resulting from a call to \code{\link{btfit}}. 
}
  \item{ref}{
Either \code{NULL} (the default), or the length-1 index to specify an element of the vector \code{object $ pi} as the reference value.  If \code{NULL} is used, the constraint used is the default for \code{\link{btfit}}, namely that  \code{sum(object $ pi)} is set equal to 1.
}
  \item{\dots}{
Other possible arguments to the \code{\link{summary}} function.
}
}
\value{
A data frame with components (columns):
 \item{estimate }{Estimated log-abilities}
 \item{SE }{Estimated standard errors}
%% ...
}
\author{
David Firth
}
\examples{
library(BradleyTerry2)
data(citations)
citations_model <- btfit(citations, 1, epsilon = 1e-8)
summary(citations_model, ref = 1)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ models }% use one of  RShowDoc("KEYWORDS")

