# Converts a graph representation of wins into a square matrix.
graph_to_matrix <- function(g) {
  
  # check that graph is a directed igraph object
  if(!igraph::is.igraph(g))  stop("g must be a directed igraph object")
  if(!igraph::is.directed(g))  stop("g must be a directed igraph object")
  
  # check names
  if(!is.null(igraph::V(g)$name)) {
    
    arg <- deparse(substitute(g))
    
    if(anyDuplicated(igraph::V(g)$name) > 0) stop(paste0("Vertex names must be unique. Consider fixing with V(", arg, ")$name <- make.names(V(", arg, ")$name, unique = TRUE)"))
  }
  
  if (igraph::is.weighted(g)) W <- igraph::as_adjacency_matrix(g, sparse = TRUE, attr = "weight", names = TRUE)
  else W <- igraph::as_adjacency_matrix(g, sparse = TRUE, names = TRUE)
  
  #if (igraph::is.weighted(g)) W <- igraph::as_adjacency_matrix(g, sparse = TRUE, attr = "weight")
  #else W <- igraph::as_adjacency_matrix(g, sparse = TRUE)
  
  
  return(W)
  
}

# Converts a data frame of paired results into a square matrix.
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
  
  # get base data
  items <- sort(base::union(df[[1]], df[[2]]))
  n <- length(items)
  
  # get formula for dMcast
  f <- stats::as.formula(paste(names(df)[1:2], collapse= " ~ "))
  
  # create cross-tabs matrix (not square)
  if(!is.factor(df[,1])) {
    df[,1] <- factor(df[,1])
  }
  
  if(!is.factor(df[,2])) {
    df[,2] <- factor(df[,2])
  }
  
  mat <- Matrix.utils::dMcast(df, f, value.var = names(df)[3], as.factors = TRUE)
  
  # fix colnames
  colnames(mat) <- stringr::str_replace(colnames(mat), names(df)[2], "")
  
  # remove zeros, if any, taking care with dimnames
  summary_mat <- Matrix::summary(mat)
  x <- NULL # hack to avoid CRAN note
  if (any(summary_mat[,3] == 0)) {
    summary_mat <- dplyr::filter(summary_mat, x != 0)
    
    mat_rownames <- rownames(mat)
    mat_colnames <- colnames(mat)
    
    new_mat_rownames <- mat_rownames[sort(unique(summary_mat[,1]))]
    new_mat_colnames <- mat_colnames[sort(unique(summary_mat[,2]))]
    
    mat <- Matrix::sparseMatrix(i = summary_mat[,1], j = summary_mat[,2], x = summary_mat[,3])
    
    nonzero_rows <- which(Matrix::rowSums(mat) != 0)
    nonzero_cols <- which(Matrix::colSums(mat) != 0)
    
    mat <- mat[nonzero_rows, nonzero_cols]
    dimnames(mat) <- list(new_mat_rownames, new_mat_colnames)
  }
  
  
  # add in zeros for missing rows
  if (nrow(mat) < n) {
    new_rows <- Matrix::Matrix(0, n - nrow(mat), ncol(mat),
                               dimnames = list(base::setdiff(items, rownames(mat)), colnames(mat)))
    mat <- rbind(mat, new_rows)
  }
  
  # add in zeros for missing columns
  if (ncol(mat) < n) {
    new_cols <- Matrix::Matrix(0, n, n - ncol(mat),
                               dimnames = list(rownames(mat), base::setdiff(items, colnames(mat))))
    mat <- cbind(mat, new_cols)
  }
  
  # get rows and columns in same, sorted order and return
  mat <- mat[items,]
  mat <- mat[, rownames(mat)]
  
  # repeat above steps if in 4-column format (for item2 beating item1)
  if (ncol(df) == 4) {
    f2 <- stats::as.formula(paste(names(df)[2:1], collapse= " ~ "))
    mat2 <- Matrix.utils::dMcast(df, f2, value.var = names(df)[4], as.factors = TRUE)
    colnames(mat2) <- stringr::str_replace(colnames(mat2), names(df)[1], "")
    
    
    # remove zeros, if any, taking care with dimnames
    summary_mat2 <- Matrix::summary(mat2)
    if (any(summary_mat2[,3] == 0)) {
      summary_mat2 <- dplyr::filter(summary_mat2, x != 0)
      
      mat2_rownames <- rownames(mat2)
      mat2_colnames <- colnames(mat2)
      
      new_mat2_rownames <- mat2_rownames[sort(unique(summary_mat2[,1]))]
      new_mat2_colnames <- mat2_colnames[sort(unique(summary_mat2[,2]))]
      
      mat2 <- Matrix::sparseMatrix(i = summary_mat2[,1], j = summary_mat2[,2], x = summary_mat2[,3])
      
      nonzero_rows2 <- which(Matrix::rowSums(mat2) != 0)
      nonzero_cols2 <- which(Matrix::colSums(mat2) != 0)
      
      mat2 <- mat2[nonzero_rows2, nonzero_cols2]
      dimnames(mat2) <- list(new_mat2_rownames, new_mat2_colnames)
    }
    
    # add in zeros for missing rows
    if (nrow(mat2) < n) {
      new_rows2 <- Matrix::Matrix(0, n - nrow(mat2), ncol(mat2),
                                  dimnames = list(base::setdiff(items, rownames(mat2)), colnames(mat2)))
      mat2 <- rbind(mat2, new_rows2)
    }
    
    # add in zeros for missing columns
    if (ncol(mat2) < n) {
      new_cols2 <- Matrix::Matrix(0, n, n - ncol(mat2),
                                  dimnames = list(rownames(mat2), base::setdiff(items, colnames(mat2))))
      mat2 <- cbind(mat2, new_cols2)
    }
    
    # get rows and columns in same, sorted order and return
    mat2 <- mat2[items,]
    mat2 <- mat2[, rownames(mat2)]
    
    # add the result to mat
    mat <- mat + mat2
  }
  
  if(!is.null(colnames(df)[1]) & !is.null(colnames(df)[2])) names(dimnames(mat)) <- colnames(df)[1:2]
  
  return(mat)
}



#' Create a btdata object
#' 
#' Creates a btdata object, primarily for use in the \link{btfit} function.
#' 
#' The \code{x} argument to \code{btdata} can be one of four types:
#' 
#' \itemize{
#' 
#' \item{A matrix (either a base \code{matrix}) or a class from the \code{Matrix} pacakge), dimension \emph{K} by \emph{K}, where \eqn{K} is the number of items. The \eqn{i,j}-th element is \eqn{w_{ij}}, the number of times item \eqn{i} has beaten item \eqn{j}. Ties can be accounted for by assigning half a win (i.e. 0.5) to each item.}
#' \item{A contingency table of class \code{table}, similar to the matrix described in the above point.}
#' \item{An \code{igraph}, representing the \emph{comparison graph}, with the \eqn{K} items as nodes. For the edges:
#' \itemize{
#' \item{If the graph is unweighted, a directed edge from node \eqn{i} to node \eqn{j} for every time item \eqn{i} has beaten item \eqn{j}}
#' \item{If the graph is weighted, then one edge from node \eqn{i} to node \eqn{j} if item \eqn{i} has beaten item \eqn{j} at least once, with the weight attribute of that edge set to the number of times \eqn{i} has beaten \eqn{j}.}
#' }}
#' \item{
#' If \code{x} is a data frame, it must have three or four columns:
#' \itemize{
#' \item{3-column data frame}{The first column contains the name of the winning item, the second column contains the name of the losing item and the third columns contains the number of times that the winner has beaten the loser. Multiple entries for the same pair of items are handled correctly. If \code{x} is a three-column dataframe, but the third column gives a code for who won, rather than a count, see \code{\link{codes_to_counts}}.}
#' \item{4-column data frame}{The first column contains the name of item 1, the second column contains the name of item 2, the third column contains the number of times that item 1 has beaten item 2 and the fourth column contains the number of times item 2 has beaten item 1. Multiple entries for the same pair of items are handled correctly. This kind of data frame is also the output of \code{\link{codes_to_counts}}.}
#' \item{In either of these cases, the data can be aggregated, or there can be one row per comparison.}
#' \item{Ties can be accounted for by assigning half a win (i.e. 0.5) to each item.}
#' }
#' }
#' 
#' }
#' 
#' \code{summary.btdata} shows the number of items, the density of the \code{wins} matrix and whether the underlying comparison graph is fully connected. If it is not fully connected, \code{summary.btdata} will additional show the number of fully-connected components and a table giving the frequency of components of different sizes. For more details on the comparison graph, and how its structure affects how the Bradley-Terry model is fit, see \code{\link{btfit}} and the vignette: vignette("BradleyTerryScalable", package = "BradleyTerryScalable").
#' 
#' @param x The data, which is either a three- or four-column data frame, a directed igraph object, a square matrix or a square contingency table. See Details.
#' @param return_graph Logical. If TRUE, an igraph object representing the comparison graph will be returned.
#' @return An object of class "btdata", which is a list containing:
#' \item{wins}{A \eqn{K*K} square matrix, where the \eqn{i,j}-th element is the number of times item \eqn{i} has beaten item \eqn{j}. If the items in \code{x} are unnamed, the wins matrix will be assigned row and column names 1:K.}
#' \item{components}{A list of the fully-connected components.}
#' \item{graph}{The comparison graph of the data (if return_graph = TRUE). See Details.}
#' @seealso \code{\link{select_components}} \code{\link{codes_to_counts}}
#' @author Ella Kaye
#' @export
btdata <- function(x, return_graph = FALSE) {
  
  # check x is of an appropriate type
  
  ## Get wins matrix
  
  # if x is a table, convert it to a matrix
  if (is.table(x)) {
    attr(x, "class") <- NULL
    attr(x, "call") <- NULL
  }

  # if x is a df
  if (is.data.frame(x)) {
    if (!(ncol(x) %in% 3:4 )) stop("If x is a dataframe, it must have 3 or 4 columns.")
    wins <- pairs_to_matrix(x)
    g <- igraph::graph.adjacency(wins, weighted = TRUE, diag = FALSE)
  }
  
  # if x is a graph
  else if (igraph::is.igraph(x)) {
    if(!igraph::is.directed(x))  stop("If x is a graph, it must be a directed igraph object")
    
    # check for names
    if(!is.null(igraph::V(x)$name)) {
      
      arg <- deparse(substitute(x))
      
      if(anyDuplicated(igraph::V(x)$name) > 0) stop(paste0("If x is a graph, vertex names must be unique. Consider fixing with V(", arg, ")$name <- make.names(V(", arg, ")$name, unique = TRUE)"))
    }
    
    wins <- graph_to_matrix(x)
    g <- x
  }
  
  else if ((methods::is(x, "Matrix") | is.matrix(x) )) {
    
    # check dimensions/content
    if (dim(x)[1] != dim(x)[2]) stop("If x is a matrix or table, it must be a square")
    if(is.matrix(x)) {if (!is.numeric(x)) stop("If x is a matrix or table, all elements must be numeric")}
    if(methods::is(x, "Matrix")) {if (!is.numeric(as.vector(x))) stop("If x is a matrix or table, all elements must be numeric")}
    if (any(x < 0)) stop("If x is a matrix or table, all elements must be non-negative")
    if(!identical(rownames(x), colnames(x))) stop("If x is a matrix or table, rownames and colnames of x should be the same")
    if (anyDuplicated(rownames(x)) > 0) {
     
      arg <- deparse(substitute(x))
      stop("If x is a matrix or table with row- and column names, these must be unique. Consider fixing with rownames(", arg, ") <- colnames(", arg, ") <- make.names(rownames(", arg, "), unique = TRUE)")
    }
    
    # ensure wins is a dgCMatrix
    if (is.matrix(x)) wins <- Matrix::Matrix(x, sparse = TRUE)
    else wins <- x
    if (class(wins) != "dgCMatrix") wins <- methods::as(wins, "dgCMatrix")
    g <- igraph::graph.adjacency(wins, weighted = TRUE, diag = FALSE)
  }
  
  else stop("x must be a 3 or 4 column dataframe, a directed igraph object, or square matrix or contingency table.") 
  
  
  ## get components
  comp <- igraph::components(g, mode = "strong")
  components <- igraph::groups(comp)
  
  # name the rows and columns of the wins matrix, if NULL
  if (is.null(unlist(dimnames(wins)))) {
    K <- nrow(wins)
    dimnames(wins) <- list(1:K, 1:K)
  }
  
  # return
  result <- list(wins = wins, components = components)
  if (return_graph) result$graph <- g
  class(result) <- c("btdata", "list")
  result
}

#' @rdname btdata
#' @param object An object of class "btdata", typically the result \code{ob} of \code{ob <- btdata(..)}. 
#' @param ... Other arguments
#' @export
summary.btdata <- function(object, ...){
  if (!inherits(object, "btdata")) stop("object should be a 'btdata' object")
  K <- nrow(object$wins)
  num_comps <- length(object$components)
  connected <- num_comps == 1
  components_greater_than_one <- Filter(function(x) length(x) > 1, object$components)
  my_tab <- table(sapply(object$components, length))
  my_df <- as.data.frame(my_tab)
  #my_df <- as.data.frame(rbind(as.integer(names(my_tab)), unname(my_tab)))
  #rownames(my_df) <- c("Num. players in component:", "Number of components:")
  #colnames(my_df) <- NULL
  
  colnames(my_df) <- c("Component size", "Freq")
  
  density <- Matrix::mean(object$wins != 0)
  
  cat("Number of items:", K, "\n")
  cat("Density of wins matrix:", density, "\n")
  cat("Fully-connected:", connected, "\n")

  
  if (num_comps > 1) {
    cat("Number of fully-connected components:", num_comps, "\n")
    cat("Summary of fully-connected components: \n")
    print(my_df)
    #cat("Without singleton components... \n")
    #cat("Number of fully-connected components with two or more players:", length(components_greater_than_one), "\n")
    #cat("Number of players across fully-connected components with two or more players", )
  }
}


