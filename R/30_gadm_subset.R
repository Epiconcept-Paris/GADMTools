# gadm_subset.gadm_sf ----------------------------------------------------------
# ==============================================================================
gadm_subset.gadm_sf <- function(x, level=NULL, regions=NULL, usevar=NULL) {
  if (is.null(level)) {
    level <- x$level
  }
  if (is.null(regions)) {
    stop("Missing value for regions")
  }
  
  
  # Select name of column ---------------------------------------------------
  if (is.null(usevar)) {
    NAME <- gadm_getLevelName(x, level)
  } else {
    NAME <- usevar
  }
  
  df1 <- as.data.frame(x$sf[, NAME])
  colnames(df1) <- c("N")
  x$sf <- x$sf[df1$N %in% regions,]
  
  x
}


# gadm_subset.gadm_sp -----------------------------------------------------
# ==============================================================================
gadm_subset.gadm_sp <- function(x, level=NULL, regions=NULL, usevar=NULL) {
  if (is.null(level)) {
    level <- x$level 
  }
  if (is.null(regions)) {
    stop("Missing value for regions")
  }
  
  
  # Select name of column ---------------------------------------------------
  if (is.null(usevar)) {
    NAME <- gadm_getLevelName(x, level)
  } else {
    NAME <- usevar
  }
  
  df1 <- as.data.frame(x$spdf[, NAME])
  colnames(df1) <- c("N")
  df2 <- x$spdf[df1$N %in% regions,];
  structure(list("basename"=x$basename,
                 "spdf"=df2,
                 "level"=x$level,
                 "L360" = FALSE,
                 "stripped"=FALSE,
                 "hasBGND"  = FALSE),
            class = "gadm_sp")  
}

