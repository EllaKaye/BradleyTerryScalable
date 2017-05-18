#' @export
btdata <- function(x) {
  
  # check x is of an appropriate type
  
  ## Get wins matrix

  # if x is a df
  if (is.data.frame(x)) {
    if (!(ncol(x) %in% 3:4 )) stop("If x is a dataframe, it must have 3 or 4 columns.")
    W <- pairs_to_matrix(x)
    g <- igraph::graph.adjacency(W, weighted = TRUE, diag = FALSE)
  }
  
  # if x is a graph
  else if (igraph::is.igraph(x)) {
    if(!igraph::is.directed(x))  stop("If x is a graph, it must be a directed igraph object")
    W <- graph_to_matrix(x)
    g <- x
  }
  
  else if ((methods::is(x, "Matrix") | is.matrix(x) )) {
    
    # check dimensions/content
    if (dim(x)[1] != dim(x)[2]) stop("If x is a matrix, it must be a square")
    if(is.matrix(x)) {if (!is.numeric(x)) stop("If x is a matrix, all elements must be numeric")}
    if(methods::is(x, "Matrix")) {if (!is.numeric(as.vector(x))) stop("If x is a matrix, all elements must be numeric")}
    if (any(x < 0)) stop("If x is a matrix, all elements must be non-negative")
    
    # ensure W is a dgCMatrix
    if (is.matrix(x)) W <- Matrix::Matrix(x, sparse = TRUE)
    else W <- x
    if (class(W) != "dgCMatrix") W <- methods::as(W, "dgCMatrix")
    g <- igraph::graph.adjacency(W, weighted = TRUE, diag = FALSE)
  }
  
  else stop("x must be a square matrix, a directed igraph object, or a 3 or 4 column dataframe.") 
  
  
  ## get components
  comp <- igraph::components(g, mode = "strong")
  components <- igraph::groups(comp)
  
  # return
  result <- list(wins = W, components = components)
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


