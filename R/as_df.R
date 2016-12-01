as_df <- function(sM) {

  if(class(sM) != "dgTMatrix") sM <- as(sM, "dgTMatrix")

  if (!is.null(rownames(sM)) & !is.null(colnames(sM))) {
    df <- data.frame(player1 = rownames(sM)[sM@i + 1], player2 = rownames(sM)[sM@j + 1], expected_wins = sM@x)
  }

  else {
    df <- data.frame(player1 = sM@i + 1, player2 = sM@j + 1, num_wins = sM@x)

  }

  return(df)
}
