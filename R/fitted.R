my_diag <- function(x,y) {
  diag(x) <- y
  return(x)
}

my_fitted <- function(pi, N) {
  p <- btprob_vec(pi)
  p * N
}

as_df <- function(sM, N) {

  # get upper triangle
  sM[lower.tri(sM, diag = TRUE)] <- 0
  N[lower.tri(N, diag = TRUE)] <- 0

  if(class(sM) != "dgTMatrix") sM <- as(sM, "dgTMatrix")

  if (!is.null(rownames(sM)) & !is.null(colnames(sM))) {
    #df <- data.frame(winner = rownames(sM)[sM@i + 1], loser = rownames(sM)[sM@j + 1], fit = sM@x)

    df <- data.frame(player1 = rownames(sM)[sM@i + 1], player2 = rownames(sM)[sM@j + 1],
                     fit1 = sM@x, fit2 = N@x - sM@x)
  }

  else {
    df <- data.frame(player1 = sM@i + 1, player2 = sM@j + 1, fit1 = sM@x, fit2 = N@x - sM@x)

  }

  if(!is.null(names(dimnames(N)))) {
    if(!is.na(names(dimnames(N))[1])) colnames(df)[1] <- names(dimnames(N))[1]
    if(!is.na(names(dimnames(N))[2])) colnames(df)[2] <- names(dimnames(N))[2]
  }

  return(df)
}


#' @export
fitted.btfit <- function(btfit, as_df = FALSE){
  if (!inherits(btfit, "btfit")) stop("Argument should be a 'btfit' object")

  pi <- btfit$pi
  N <- btfit$N
  diagonal <- btfit$diagonal

  if (is.list(pi)) {

    out <- Map(my_fitted, pi, N)

    out <- Map(my_diag, out, diagonal)

    if (as_df) {
      out <- Map(as_df, out, N)
    }

  }

  else {
    p <- btprob_vec(pi)
    out <- p * N
    diag(out) <- diagonal

    if (as_df) out <- as_df(out, N)
  }

  return(out)

}
