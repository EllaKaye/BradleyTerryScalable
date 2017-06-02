# a file of helper functions used in more that one main function across the package

# assigns names to a vector (useful with purrr)
name_vec_function <- function(x, y) {
  names(x) <- y 
  return(x)
}

# assigns dimnames to a matrix (useful with purrr)
name_matrix_function <- function(x, y) {
  dimnames(x) <- list(y,y)
  return(x)
}

# assigns names of dimnames to a matrix (useful with purrr)
name_dimnames_function <- function(x, names_dimnames) {
  names(dimnames(x)) <- names_dimnames
  return(x)
}

# assigns the diagonal of a matrix (useful with purrr)
my_diag <- function(x,y) {
  diag(x) <- y
  return(x)
}

# renames the first two columns of a data frame (defaults to "item1" and "item2")
df_col_rename_func <- function(df, names_dimnames) {
  
  colnames(df)[1:2] <- c("item1", "item2")
  
  if(!is.null(names_dimnames)) {
    if(!is.na(names_dimnames[1])) colnames(df)[1] <- names_dimnames[1]
    if(!is.na(names_dimnames[2])) colnames(df)[2] <- names_dimnames[2]
  }
  
  return(df)
}

# checks that the value for ref if valid - will stop or default to ref = NULL
# used in vcov.btfit, coef.btfit and summary.btfit
#' @export
ref_check <- function(ref, pi) {
  if (!is.null(ref)) {
    if (is.character(ref)) {
      if (length(ref) != 1) stop("'ref' should be the name of an item, 1, or NULL")
      names <- purrr::map(pi, ~ names(.x)) 
      names <- unlist(names)
      if (!(ref %in% names)) {
        ref <- NULL
        warning("The value of 'ref' is not an item name. Using ref = NULL instead")
      }
      
    }
    else if (is.numeric(ref)) {
      if (length(ref) != 1) stop("'ref' should be the name of an item, 1, or NULL")
      if (ref != 1) stop("'ref' should be the name of an item, 1, or NULL")
    }
    else stop("invalid value of ref")
  }
  
  ref
}



