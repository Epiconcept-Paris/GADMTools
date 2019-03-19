# gadm_remove.gadm_sf ----------------------------------------------------------
# ==============================================================================
gadm_remove.gadm_sf <- function(x, level=NULL, regions=NULL) {
  if (is.null(level)) {
    level <- x$level
  }
  
  if (is.null(regions)) {
    stop("Missing value for regions")
  }
  
  l_name <- gadm_getLevelName(x, level)
  l_sf <- as.data.frame(x$sf)
  l_sf <- l_sf[!l_sf[, l_name] %in% regions, ] 
  x$sf <- sf::st_as_sf(l_sf)
  
  x
}

# gadm_remove.gadm_sp ----------------------------------------------------------
# ==============================================================================
gadm_remove.gadm_sp <- function(x, level=NULL, regions=NULL) {
  if (is.null(level)) {
    level <- x$level 
  }
  if (is.null(regions)) {
    stop("Missing value for regions")
  }
  NAME <- sprintf("NAME_%d", level)
  df1 <- as.data.frame(x$spdf[, NAME])
  colnames(df1) <- c("N")
  df2 <- x$spdf[!df1$N %in% regions,];
  structure(list("basename"=x$basename,
                 "spdf"=df2,
                 "level"=x$level,
                 "L360" = x$L360,
                 "stripped"=FALSE,
                 "hasBGND"  = FALSE),
            class = "gadm_sp")  
}

