gadm_removeBackground.gadm_sf <- function(x) {
  if (x$hasBGND == FALSE) {
    warning("Map has no background.")
    return(x)
  }
  
  x$hasBGND <- FALSE
  x$BGND    <- NULL
  
  x
}

gadm_removeBackground.gadm_sp <- function(x) {
  if (x$hasBGND == FALSE) {
    warning("Map has no background.")
    return(x)
  }
  
  x$hasBGND <- FALSE
  x$BGND    <- NULL
  
  x
}

