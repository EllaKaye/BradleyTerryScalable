as_df <- function(sM) {

  if(class(sM) != "dgTMatrix") sM <- as(sM, "dgTMatrix")

  if (!is.null(rownames(sM)) & !is.null(colnames(sM))) {
    df <- data.frame(winner = rownames(sM)[sM@i + 1], loser = rownames(sM)[sM@j + 1], fit = sM@x)
  }

  else {
    df <- data.frame(winner = sM@i + 1, loser = sM@j + 1, fit = sM@x)

  }

  return(df)
}

as_df1 <- function(fitted, N = 0, diag = 0) {
  fitted[lower.tri(fitted, diag = TRUE)] <- 0

  #N[lower.tri(N, diag = TRUE)] <- 0

  as_df(fitted) # gives upper triangle

  fitted
}


as_df2 <- function(sM, N) {

  if(class(sM) != "dgTMatrix") sM <- as(sM, "dgTMatrix")

  if (!is.null(rownames(sM)) & !is.null(colnames(sM))) {
    df <- data.frame(player1 = rownames(sM)[sM@i + 1], player2 = rownames(sM)[sM@j + 1],
                     fit1 = sM@x, fit2 = N@x - sM@x)
  }

  else {
    df <- data.frame(winner = sM@i + 1, loser = sM@j + 1, fit = sM@x)

  }

  return(df)
}


