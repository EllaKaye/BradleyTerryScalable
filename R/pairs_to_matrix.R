#' Converts a data frame of paired results into a square matrix.
#'
#' \code{pairs_to_matrix} is a data manipulation function that converts a data frame of paired results (either ordered or unordered) into a square matrix. The output is of the correct form to be passed as the \code{W} argument into the \code{\link{connected_components}} and \code{\link{btfit}} functions in this package.
#'
#' @param df A data frame (either a \code{\link[base]{data.frame}} or an object that inherits that class, such as a \code{\link[dplyr]{tbl_df}} or a \code{\link[data.table]{data.table}}). It must have 3 or 4 columns:
#' \describe{
#'   \item{Ordered}{If a 3-column data-frame, the first column contains the name of the winning player, the second column contains the name of the losing player and the third columns contains the number of times that the winner has beaten the loser.}
#'   \item{Unordered}{If a 4-column data-frame, the first column contains the name of Player 1, the second column contains the name of Player 2, the third column contains the number of times that Player 1 has beaten Player 2 and the fourth column contains the number of times Player 2 has beaten Player 1.}
#' }
#' In both cases, if there is a repeated pairing in \code{df}, \code{pairs_to_matrix} aggregates the number of wins, as one would expect.
#' @return A \eqn{K} by \eqn{K} sparse Matrix, where \eqn{K} is the number of players. The \eqn{i,j}-th element of this matrix is the number of times player \eqn{i} beats player \eqn{j}. This output is in the correct format to pass as the \code{W} argument to \code{\link{connected_components}} and \code{\link{btfit}}.
#' @seealso \code{\link{connected_components}}, \code{\link{btfit}}.
#' @examples
#' winner <- c("A", "A", "A", "A", "A", "B", "B", "B", "C", "C", "E", "F", "F")
#' loser <- c("B", "B", "C", "D", "F", "D", "A", "F", "F", "G", "G", "A", "C")
#' df <- data.frame(winner = winner, loser = loser, num_wins = 1:13)
#' pairs_to_matrix(df)
#'
#' df2 <- data.frame(team1 = c("A", "A", "B", "C"),
#'                   team2 = c("B", "C", "C", "D"),
#'                   wins1 = 1:4, wins2 = 5:8)
#' pairs_to_matrix(df2)

#' @export
pairs_to_matrix <- function(df) {

  # Check for Matrix.utils
  if (!requireNamespace("Matrix.utils", quietly = TRUE)) {
    stop("The package Matrix.utils is needed for this function to work. Please install it.",
         call. = FALSE)
  }

  # Check for stringr
  if (!requireNamespace("stringr", quietly = TRUE)) {
    stop("The package stringr is needed for this function to work. Please install it.",
         call. = FALSE)
  }

  # check if data frame
  if(!(is.data.frame(df))) stop ("Argument must be a data frame")

  # ensure df is a data.frame (rather than tbl_df or tbl)
  class(df) <- "data.frame"

  # check number of columns
  if (!(ncol(df) %in% 3:4 )) stop("Argument must be a data frame with three or four columns")

  # get formula for dMcast
  f <- stats::as.formula(paste(names(df)[1:2], collapse= " ~ "))

  # create cross-tabs matrix (not square)
  mat <- Matrix.utils::dMcast(df, f, value.var = names(df)[3], as.factors = TRUE)

  # fix colnames
  colnames(mat) <- stringr::str_replace(colnames(mat), names(df)[2], "")

  # get ready to make square
  players <- sort(base::union(rownames(mat), colnames(mat)))
  n <- length(players)

  # add in zeros for missing rows
  if (nrow(mat) < n) {
    new_rows <- Matrix::Matrix(0, n - nrow(mat), ncol(mat),
                       dimnames = list(base::setdiff(players, rownames(mat)), colnames(mat)))
    mat <- rbind(mat, new_rows)
  }

  # add in zeros for missing columns
  if (ncol(mat) < n) {
    new_cols <- Matrix::Matrix(0, n, n - ncol(mat),
                       dimnames = list(rownames(mat), base::setdiff(players, colnames(mat))))
    mat <- cbind(mat, new_cols)
  }

  # get rows and columns in same, sorted order and return
  mat <- mat[players,]
  mat <- mat[, rownames(mat)]

  # repeat above steps if in 4-column format (for player2 beating player1)
  if (ncol(df) == 4) {
    f2 <- stats::as.formula(paste(names(df)[2:1], collapse= " ~ "))
    mat2 <- Matrix.utils::dMcast(df, f2, value.var = names(df)[4], as.factors = TRUE)
    colnames(mat2) <- stringr::str_replace(colnames(mat2), names(df)[1], "")

    # add in zeros for missing rows
    if (nrow(mat2) < n) {
      new_rows2 <- Matrix::Matrix(0, n - nrow(mat2), ncol(mat2),
                          dimnames = list(base::setdiff(players, rownames(mat2)), colnames(mat2)))
      mat2 <- rbind(mat2, new_rows2)
    }

    # add in zeros for missing columns
    if (ncol(mat2) < n) {
      new_cols2 <- Matrix::Matrix(0, n, n - ncol(mat2),
                          dimnames = list(rownames(mat2), base::setdiff(players, colnames(mat2))))
      mat2 <- cbind(mat2, new_cols2)
    }

    # get rows and columns in same, sorted order and return
    mat2 <- mat2[players,]
    mat2 <- mat2[, rownames(mat2)]

    # add the result to mat
    mat <- mat + mat2
  }
  
  if(!is.null(colnames(df)[1]) & !is.null(colnames(df)[2])) names(dimnames(mat)) <- colnames(df)[1:2]
  
  return(mat)
}
