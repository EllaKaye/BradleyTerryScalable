#' Statistics Journal Citation Data from Stigler (1994)
#'
#' Extracted from a larger table in Stigler (1994). Inter-journal citation counts for four journals, "Biometrika", "Comm Statist.", "JASA" and "JRSS-B", as used on p448 of Agresti (2002)
#'
#' In the context of paired comparisons, the 'winner' is the cited journal and the 'loser' is the one doing the citing.
#' 
#' This dataset also appears in the \code{BradleyTerry2} package.
#' 
#' @source Agresti, A. (2002) \emph{Categorical Data Analysis} (2nd ed.). New York: Wiley
#' @references Stigler, S. (1994) Citation patterns in the journals of statistics and probability. \emph{Statistical Science}, \strong{9}, 384-406.
#' @format A four by four matrix, where the \eqn{i,j}-th element is the number of times journal \eqn{i} has been cited by journal \eqn{j}.
"citations"

#' A toy data set for the \code{BradleyTerryScalable} package
#' 
#' A toy data set, where the underlying comparison graph of the players is not fully connected. Each row represents one game.
#' 
#' @format A data frame with 13 rows and 3 variables:
#' \describe{
#'   \item{player1}{The name of player1}
#'   \item{player2}{The name of player2}
#'   \item{outcome}{Outcome of the game: \code{"W1"} if player1 beats player2, \code{"W2"} if player2 beats player2 and \code{"D"}if it was a draw.}
#' }
#' 
"toy_data"