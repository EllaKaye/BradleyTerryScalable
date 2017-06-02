.onLoad <- function(libname = find.package("BradleyTerryScalable"), pkgname = "BradleyTerryScalable"){
  
  # CRAN Note avoidance
  if(getRversion() >= "2.15.1") 
    utils::globalVariables(
      c("value", "prob1wins", "estimate", "num_items")
    )
  invisible()
}

.onUnload <- function (libpath) {
  library.dynam.unload("BradleyTerryScalable", libpath)
}