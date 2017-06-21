.onLoad <- function(libname = find.package("BradleyTerryScalable"), pkgname = "BradleyTerryScalable"){
  
  # CRAN Note avoidance
  if(getRversion() >= "2.15.1") 
    utils::globalVariables(
      c("Freq", "prob1wins", "estimate", "num_items", "item", "SE", "wins_code", "item1wins", "item2wins")
    )
  invisible()
}

.onUnload <- function (libpath) {
  library.dynam.unload("BradleyTerryScalable", libpath)
}