gadm.getBackground.GT2 <- function(x, name, type="osm", clip=TRUE) {
  
  # Settings ----------------------------------------------------------------
  .shapeBoundaries <- gadm.union(x)
  # .sp$spdf  <- as(x$spdf, "Spatial")
  .shapeBoundaries$spdf  <- as(.shapeBoundaries$spdf, "Spatial")
  # .shapeBoundaries <- gadm.union(.sp)
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

  # Create GADMWrapper object -----------------------------------------------
  structure(list("basename" = x$basename,
                 "spdf"     = x$spdf,
                 "level"    = x$level,
                 "L360"     = x$L360,
                 "stripped" = x$stripped,
                 "hasBGND"  = TRUE,
                 "BGND"     = .df),
              class = "GT2")

}
