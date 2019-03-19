
choropleth.gadm_sp <- function(x,
                                   data,
                                   value=NULL,
                                   breaks = NULL,
                                   steps = 5,
                                   adm.join = NULL,
                                   legend = NULL,
                                   labels = NULL,
                                   palette=NULL,
                                   title="",
                                   subtitle = NULL,
                                   caption  = NULL) {
  .x <- x
  .subtitle <- subtitle
  .caption <- caption
  .titles <- title
  
  if (is.null(value)) stop("Unknown value (NULL)\n")
  
  if (is.null(adm.join)) {
    stop("adm.join is NULL! You MUST provide a name.")
  }
  
  if (.x$hasBGND == TRUE) {
    .raster <- x$BGND
  }
  
  .data <- data
  .value <- value
  .range <- range
  .legend <- legend
  .palette <- palette
  .steps <- steps
  .labels <- labels
  
  # get breaks ----------------------------------------------------
  .data <- internal.gt2.getBreaks(.data, .value, breaks, .steps, .labels)

  .levelName <- gadm_getLevelName(.map, x$level)
  
  if (!is.null(adm.join)) {
    names(.data)[names(.data)==adm.join] <- .levelName
    .map <- fortify(x$spdf, region=.levelName)
    if (x$stripped == FALSE) {
      .map <- splitShapes(x, .levelName)
    }
    names(.map)[names(.map)=="id"] <- .levelName
  }
  
  P <- dplyr::left_join(.map, .data, by = .levelName)
  
  if (!is.factor(P[,value])) {
    P[,value] <- as.numeric(P[,value])
  }
  
  names(P)[names(P)==value] <- "CHPLT_VALUE"

  
  # Palettes ----------------------------------------------------------------
  if (is.null(palette)) {
    .palette <- rev(RColorBrewer::brewer.pal(9, "Spectral"))
  }
  else {
    if (length(palette)==1) {
      .palette <- RColorBrewer::brewer.pal(9, palette)
    }
    else {
      .steps <- length(palette)
    }
  }
  
  
  if (is.null(labels)) {
    .labels <- levels(P$CHPLT_VALUE)
  }
  
  if (is.null(legend)) .legend <- value
  
  long <- lat <- group <- x <- y <- CHPLT_VALUE <- NULL
  
  
  # Plot map ----------------------------------------------------------------
  .P <- ggplot()
  
  
  # Draw background if exists -----------------------------------------------
  if (.x$hasBGND) {
    .P <- .P + geom_raster(data=.raster, aes(x, y), fill=.raster$rgb)
  }
  
  # Draw the shapefile ------------------------------------------------------
  
  .P <- .P + geom_polygon(data=P,
                          aes(x=long, y=lat, group=group, fill=CHPLT_VALUE),
                          color = "black", size = 0.25) +
    
    
    scale_fill_manual(.legend, values = .palette, 
                      limits=levels(P$CHPLT_VALUE),
                      labels=.labels,
                      guide = guide_legend(reverse = TRUE)) +
    
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
  if (.legend == FALSE) {
    .P <- .P + theme(legend.position="none")
  }
  .P
}

