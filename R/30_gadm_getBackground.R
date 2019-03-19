gadm_getBackground.gadm_sp <- function(x, name, type="osm", clip=TRUE) {
  # Settings ----------------------------------------------------------------
  .shapeBoundaries <- gadm_union(x)
  .filename <- sprintf("%s.tif", name)
  if (type %in% rosm::osm.types()) {
    .type <- type
  } else {
    .msg <- sprintf("Unknown type '%s'", type)
    stop(.msg)
  }
  
  # Get tiles & create tiff file --------------------------------------------
  if (!file.exists(.filename)) {
    .osmRaster <- rosm::osm.raster(.shapeBoundaries$spdf, type = .type)
    rosm::osm.raster(.osmRaster, filename = .filename, overwrite = TRUE)
  }
  

  # Cropping and Cliping ----------------------------------------------------
  if (file.exists(.filename)) {
    .map <- raster::stack(.filename)
    .r1 <- raster::crop(.map, raster::extent(.shapeBoundaries$spdf))
    if (clip == TRUE) {
      .r2 <- raster::mask(.r1, .shapeBoundaries$spdf)
      .map <- .r2
    } else {
      .map <- .r1
    }
  }
  
  
  # Creates a RGB data.frame for ggplot2 -----------------------------------
  .map <- .map / raster::maxValue(.map)
  .df <- raster::as.data.frame(.map, xy = TRUE)
  .df <- .df[complete.cases(.df), ]
  colnames(.df) <- c("x", "y", "r", "g", "b")
  .df$rgb <- with(.df, rgb(r, g, b,1))
  .df <- .df[, c(1,2,6)]
  
  # Create gadm_sp object -----------------------------------------------
  structure(list("basename" = x$basename,
                 "spdf"     = x$spdf,
                 "level"    = x$level,
                 "L360"     = x$L360,
                 "stripped" = x$stripped,
                 "hasBGND"  = TRUE,
                 "BGND"     = .df),
              class = "gadm_sp")
  
}  

# gadm_getBackground.gadm_sf ---------------------------------------------------
# ==============================================================================
gadm_getBackground.gadm_sf <- function(x, name, type="osm", clip=TRUE) {

  # Settings ----------------------------------------------------------------
  .shapeBoundaries <- gadm_union(x)
  # .sp$spdf  <- as(x$spdf, "Spatial")
  .shapeBoundaries$sf  <- as(.shapeBoundaries$sf, "Spatial")
  # .shapeBoundaries <- gadm_union(.sp)
  .filename <- sprintf("%s.tif", name)
  if (type %in% rosm::osm.types()) {
    .type <- type
  } else {
    .msg <- sprintf("Unknown type '%s'", type)
    stop(.msg)
  }
  
  # Get tiles & create tiff file --------------------------------------------
  if (!file.exists(.filename)) {
    .osmRaster <- rosm::osm.raster(.shapeBoundaries$sf, type = .type)
    rosm::osm.raster(.osmRaster, filename = .filename, overwrite = TRUE)
  }
  
  
  # Cropping and Cliping ----------------------------------------------------
  if (file.exists(.filename)) {
    .map <- raster::stack(.filename)
    .r1 <- raster::crop(.map, raster::extent(.shapeBoundaries$sf))
    if (clip == TRUE) {
      .r2 <- raster::mask(.r1, .shapeBoundaries$sf)
      .map <- .r2
    } else {
      .map <- .r1
    }
  }
  
  
  # Creates a RGB data.frame for ggplot2 -----------------------------------
  .map <- .map / raster::maxValue(.map)
  .df <- raster::as.data.frame(.map, xy = TRUE)
  .df <- .df[complete.cases(.df), ]
  colnames(.df) <- c("x", "y", "r", "g", "b")
  .df$rgb <- with(.df, rgb(r, g, b,1))
  .df <- .df[, c(1,2,6)]
  
  x$hasBGND <- TRUE
  x$BGND <-.df 
  
  x

}
