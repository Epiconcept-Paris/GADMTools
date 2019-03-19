gadm_showNorth <- function(plot,  where="br") {
  plot + ggspatial::annotation_north_arrow(location = where, which_north = "true",
                                           height = unit(1, "cm"), width = unit(1, "cm"),
                                           pad_x = unit(0.25, "cm"),
                                           pad_y = unit(0.25, "cm"),
                                           style = ggspatial::north_arrow_fancy_orienteering)
}


gadm_showScale <- function(plot,  where="bl") {
  plot + ggspatial::annotation_scale(location = where, width_hint = 0.25)
}
