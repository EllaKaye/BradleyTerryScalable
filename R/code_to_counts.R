#' Converts data frame with a code for wins to counts of wins
#'
#' Convert a three-column data frame in which the third column is a code representing whether the item in column 1 won, lost or (if applicable) drew over/with the item in column 2, to a dataframe with counts (suitable for use in \code{\link{btdata}})
#' 
#' This function is needed in the \code{BradleyTerryScalable} workflow when the user data is stored in a three-column data frame where each row is a comparison between two items, and where the third column is NOT a count of the number of times the item in the first column beat the item in the second column. Rather, it could be that the third column is a code for which of the two items won (including the possibility of a tie), for example \code{"W1", "W2", "D"}. Or else, it could be that the third column gives the score only in relation to the first item, e.g. 1 for a win, 0 for a loss or 0.5 for a draw without there anywhere in the table being the corresponding record for the second item (i.e. respectively 0 for a loss, 1 for a win and 0.5 for a draw.)
#' 
#' 
#' @param df A three-column data frame. Each row represents a comparison between two items. The first and second columns are the names of the first and second items respectively. The third column gives a code for which won. See Details and Examples.
#' @param codes A numeric vector or character vector, of length two or three (depending on whether there are ties.) The first and second element gives the codes used if the first or second item won respectively. If there are ties, the third element gives the code used in that case. See Details and Examples.
#' @return A four-column data frame where the first two columns are the name of the first and second item. The third and fourth column gives the wins count for the first and second item respectively: 1 for a win, 0 for a loss, and 0.5 each for a draw. This data frame is in the correct format to be passed to \code{\link{btdata}}
#' @examples 
#' first <- c("A", "A", "B", "A")
#' second <- c("B", "B", "C", "C")
#' df1 <- data.frame(player1 = first, player2 = second, code = c("W1", "W2", "D", "D"))
#' codes_to_counts(df1, c("W1", "W2", "D"))
#' df2 <- data.frame(item1 = first, item2 = second, result = c(0, 1, 1, .5))
#' codes_to_counts(df2, c(1, 0, .5))
#' df3 <- data.frame(player1 = first, player2 = second, which_won = c(1,2,2,1))
#' codes_to_counts(df3, c(1,2))
#' codes_to_counts(BradleyTerryScalable::toy_data, c("W1", "W2", "D"))
#' @author Ella Kaye
#' @seealso \code{\link{btdata}}
#'
#' @export
codes_to_counts <- function(df, codes) {
  
  # check arguments
  if (!is.data.frame(df)) stop("df must be a data frame")
  if (ncol(df) != 3) stop("df must have three columns")
  if (!(is.numeric(codes) | is.character(codes))) stop("codes must be a numeric or character vector")
  if (!(length(codes) %in% 2:3)) stop("codes must be a vector of length 2 or 3")
  
  df <- dplyr::as_data_frame(df)
  
  # extract code elements
  W1 <- codes[1]
  W2 <- codes[2]
  if (length(codes) == 3) D <- codes[3]
  
  # check that codes match content in column three
  codes_elements <- sort(unique(df[[3]]))
  if (is.character(codes)) {
    codes <- factor(codes)
    codes_elements <- factor(codes_elements)
  }
  if (!(identical(sort(codes), sort(codes_elements)))) stop("The elements in codes don't match the elements in the third column of df")
  
  # make col3 name consistent, so can use in mutate statements
  colnames(df)[3] <- "wins_code"
  
  # assign win to appropriate team
  df <- dplyr::mutate(df, item1wins = dplyr::if_else(wins_code == W1, 1, 0))
  df <- dplyr::mutate(df, item2wins = dplyr::if_else(wins_code == W2, 1, 0))  
  
  # deal with ties
  if (length(codes) == 3) {
    df <- dplyr::mutate(df, item1wins = dplyr::if_else(wins_code == D, 0.5, item1wins))
    df <- dplyr::mutate(df, item2wins = dplyr::if_else(wins_code == D, 0.5, item2wins))
  }
  
  df <- dplyr::select(df, c(1:2, 4:5))
  df 
}
