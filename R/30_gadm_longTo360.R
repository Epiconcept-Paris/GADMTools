# gadm_longTo360.gadm_sf --------------------------------------------------
# =========================================================================
gadm_longTo360.gadm_sf <- function(x) {
  .x <- x
  geom <- st_geometry(.x$sf)
  geom = (st_geometry(geom) + c(360,90)) %% c(360) - c(0,90)
  
  .x$sf$geometry <- geom
  .x
}

