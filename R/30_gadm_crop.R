# gadm_crop.gadm_sf -------------------------------------------------------
# =========================================================================
gadm_crop.gadm_sf <- function(x, xmin, ymin, xmax, ymax) {
  .x <- x
  .x$sf <- sf::st_crop(.x$sf, xmin=xmin, ymin=ymin, xmax=xmax, ymax=ymax)
  .x
}

# gadm_crop.gadm_sp -------------------------------------------------------------------------------------------
# =================================================================================================================
gadm_crop.gadm_sp <- function(x, xmin, ymin, xmax, ymax) {
  .x <- x
  .x$spdf <- raster::crop(.x$spdf, raster::extent(xmin, xmax, ymin, ymax ))
  .x
}
