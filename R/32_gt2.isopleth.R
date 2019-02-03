# isopleth <- function(x,  data, palette=NULL, title="", subtitle="", caption="") UseMethod("isopleth", x)
isopleth.GT2 <- function(x, 
                         data, 
                         palette=NULL, 
                         title="", 
                         subtitle="", 
                         caption="") {
  
  .x <- x
  .map <- x$spdf
  .subtitle <- subtitle
  .caption <- caption
  
  # Select a name to fortify it (for ggplot2) -------------------------------
  # .name <- gadm.getLevelName(x)
  
  if (.x$hasBGND == TRUE) {
    .raster <- x$BGND
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
  
  bb <- sf::st_bbox(.x$spdf)
  Xmin <- bb[[1]]
  Xmax <- bb[[3]]
  Ymin <- bb[[2]]
  Ymax <- bb[[4]]
  
  
  P <- ggplot()
  
  # Draw background if exists -----------------------------------------------
  if (.x$hasBGND) {
    P <- P + geom_raster(data=.raster, aes(x, y), fill=.raster$rgb)
  }
  
  # Draw the shapefile ------------------------------------------------------
  
  P <- P + geom_sf(data=.map, fill=NA, color="black", size = 0.5) +
    geom_point(data = .data, aes(x=longitude, y=latitude)) +
    xlim(Xmin, Xmax) + ylim(Ymin, Ymax)
  
  P <-  internal_getNorthScaleBar(P)
    
  P <- P + geom_density_2d(data = .data, aes(x=longitude, y=latitude)) +
    
    stat_density_2d(data = .data,
                    aes(x = longitude, y = latitude, 
                        fill = ..level.., alpha = ..level..), 
                    size = 10, bins = 10, geom = "polygon")+
    scale_fill_gradientn(colours = .palette(100)) +
    scale_alpha(guide = 'none') +
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
    coord_sf();
  P
}

