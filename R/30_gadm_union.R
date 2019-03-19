# gadm_union.gadm_sf ------------------------------------------------------
# =========================================================================
gadm_union.gadm_sf <- function(x, level=0, type="?") {
  
  .sf <- x$sf
  .sf <- internal_gadm_union(.sf, x$level, level, type)
  x$sf <- .sf
  x$level = level
  x
}

# gadm_union.gadm_sp ------------------------------------------------------
# =========================================================================
gadm_union.gadm_sp <- function(x, level=0, type="?") {
  
  if (x$stripped == FALSE) {
    .name <- gadm_getLevelName(x)
    x$spdf@data[, "UNIFY"] <- rep("WXY", length(x$spdf))
    .sp <- unionSpatialPolygons(x$spdf, x$spdf@data[, "UNIFY"])
    .df <- data.frame(rep("WXY", length(.sp)))
    colnames(.df) <- c(.name)
    .sp <- SpatialPolygonsDataFrame(.sp, .df, match.ID = FALSE) 
    return (structure(
      list(
        "basename" = x$basename,
        "spdf"     = .sp,
        "level"    = x$level,
        "L360"     = x$L360,
        "stripped" = FALSE,
        "hasBGND"  = FALSE
      ),
      class = "gadm_sp"
    ))
  }
  else {
    stop("ERROR: gadm_union does not works on stripped polygons.")
    return(x)
  }
}                              

