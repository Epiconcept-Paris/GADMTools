gadm.removeBackground.GT2 <- function(x) {
  if (x$hasBGND == FALSE) {
    warning("Map has no background.")
    return(x)
  }
  
  x$hasBGND <- FALSE
  x$BGND    <- NULL
  
  x
}

gadm.removeBackground.GADMWrapper <- function(x) {
  if (x$hasBGND == FALSE) {
    warning("Map has no background.")
    return(x)
  }
  
  x$hasBGND <- FALSE
  x$BGND    <- NULL
  
  x
}

