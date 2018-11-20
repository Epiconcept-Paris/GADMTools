isopleth <- function(x,  data, palette=NULL, title="", subtitle="", caption="") UseMethod("isopleth", x)
isopleth.GADMWrapper <- function(x, 
                                 data, 
                                 palette=NULL, 
                                 title="", 
                                 subtitle="", 
                                 caption="") {
  
  .x <- x
  .subtitle <- subtitle
  .caption <- caption
  
  # Select a name to fortify it (for ggplot2) -------------------------------
  .name <- gadm.getLevelName(x)
  
  if (.x$hasBGND == TRUE) {
    .raster <- x$BGND
  } 
  
  if (x$stripped == FALSE) {
    .map <- splitShapes(x, .name)
  }
  
  .data <- data
  .titles <- title
  .palette <- palette
  
  if (is.null(palette)) {
    .palette <- colorRampPalette(rev(RColorBrewer::brewer.pal(11, "Spectral")), space="Lab")
  }
  else {
    .palette <- colorRampPalette(RColorBrewer::brewer.pal(9, palette), space="Lab")
  }
  
  long = lat = longitude = latitude = x = y = group = ..level.. = NULL
  
  P <- ggplot()
  
  # Draw background if exists -----------------------------------------------
  if (.x$hasBGND) {
    P <- P + geom_raster(data=.raster, aes(x, y), fill=.raster$rgb)
  }
  
  # Draw the shapefile ------------------------------------------------------
  
  P <- P + geom_polygon(data=.map, aes(x=long, y=lat,  group=group),
                        fill=NA, color="black", size = 0.5) +
    stat_density2d(aes(x = longitude, y = latitude, fill = ..level..), 
                   alpha=0.5, size = 10, bins = 10, data = .data, geom = "polygon")+
    scale_fill_gradientn(colours = .palette(100)) +
    labs(title = .titles,
         subtitle = .subtitle,
         caption = .caption,
         fill = "") + 
    theme_bw() +
    theme(plot.title = element_text(hjust=0.5),
          plot.subtitle = element_text(hjust=0.5),
          plot.caption = element_text(hjust=0))+
    theme(panel.border = element_blank()) +
    theme(legend.key = element_blank()) +
    theme(axis.text = element_blank()) +
    theme(axis.title = element_blank()) +
    theme(axis.ticks = element_blank()) +
    coord_quickmap();
  P
}

