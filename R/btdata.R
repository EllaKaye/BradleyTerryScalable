#' Create a btdata object
#' 
#' Creates a btdata object, primarily for use in the \link{btfit} function.
#' 
#' If \code{x} is a data frame, it must have three or four columns.
#' 
#' If \code{x} is 3-column data-frame, the first column contains the name of the winning item, the second column contains the name of the losing item and the third columns contains the number of times that the winner has beaten the loser. Multiple entries for the same pair of items are handled correctly.
#' 
#' If \code{x} is a 4-column data-frame, the first column contains the name of item 1, the second column contains the name of item 2, the third column contains the number of times that item 1 has beaten item 2 and the fourth column contains the number of times item 2 has beaten item 1. Multiple entries for the same pair of items are handled correctly.
#' 
#' Alternatively, \code{x} can be an igraph object representing the comparison graph of the data, i.e. the graph where the nodes are the items, and there is a directed edge from node \eqn{i} to node \eqn{j} whenever item \eqn{i} has beaten item \eqn{j} at least once. Then, if \code{x} is a weighted graph, the weight on each edge \eqn{i-j} represents the number of times item \eqn{i} has beaten item \eqn{j}. If \code{x} is not weighted, there is a separate edge from node \eqn{i} to node \eqn{j} for each time item \eqn{i} has beaten item \eqn{j}.
#' 
#' Finally, \code{x} can be a square matrix, where the \eqn{i,j}-th element is the number of times item \eqn{i} has beaten item \eqn{j}. The items must be in the same order on the rows and the columns.
#' 
#' @param x The data, which is either a three- or four-column data frame, a directed igraph object, or a square matrix. See Details.
#' @param return_graph Logical. If TRUE, an igraph object representing the comparison graph will be returned.
#' @return A btdata object, which is a list containing:
#' \item{wins}{A K*K square matrix, where the \eqn{i,j}-th element is the number of times item \eqn{i} has beaten item \eqn{j}. If the items in \code{x} are unnamed, the wins matrix will be assigned row and column names 1:K.}
#' \item{components}{A list of the fully-connected components.}
#' \item{graph}{The comparison graph of the data (if return_graph = TRUE).}
#' @seealso \code{\link{select_components}}
#' @export
btdata <- function(x, return_graph = FALSE) {
  
  # check x is of an appropriate type
  
  ## Get wins matrix

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
    if (dim(x)[1] != dim(x)[2]) stop("If x is a matrix, it must be a square")
    if(is.matrix(x)) {if (!is.numeric(x)) stop("If x is a matrix, all elements must be numeric")}
    if(methods::is(x, "Matrix")) {if (!is.numeric(as.vector(x))) stop("If x is a matrix, all elements must be numeric")}
    if (any(x < 0)) stop("If x is a matrix, all elements must be non-negative")
    if(!identical(rownames(x), colnames(x))) stop("If x is a matrix, rownames and colnames of x should be the same")
    if (anyDuplicated(rownames(x)) > 0) {
     
      arg <- deparse(substitute(x))
      stop("If x is a matrix with row- and column names, these must be unique. Consider fixing with rownames(", arg, ") <- colnames(", arg, ") <- make.names(rownames(", arg, "), unique = TRUE)")
    }
    
    # ensure wins is a dgCMatrix
    if (is.matrix(x)) wins <- Matrix::Matrix(x, sparse = TRUE)
    else wins <- x
    if (class(wins) != "dgCMatrix") wins <- methods::as(wins, "dgCMatrix")
    g <- igraph::graph.adjacency(wins, weighted = TRUE, diag = FALSE)
  }
  
  else stop("x must be a square matrix, a directed igraph object, or a 3 or 4 column dataframe.") 
  
  
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
  
  cat("Number of players:", K, "\n")
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


