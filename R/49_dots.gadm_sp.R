
dots.gadm_sp <- function(x, points, 
                             color="red",
                             size = 8,
                             value = NULL,
                             breaks = NULL,
                             steps = 5,
                             palette = NULL,
                             labels = NULL,
                             strate = NULL ,
                             title    = "",
                             subtitle = "",
                             caption  = "",
                             legend = NULL,
                             note="") {
  
  .x <- x
  .subtitle <- subtitle
  .caption <- caption
  .titles <- title
  
  # Select a name to fortify it (for ggplot2) -------------------------------
  .name <- gadm_getLevelName(x)
  
  if (.x$hasBGND == TRUE) {
    .raster <- x$BGND
  } 
  
  if (x$stripped == FALSE) {
    .data <- splitShapes(x, .name)
  }
  
  .title <- title
  .pcolor <- color
  .value <- value
  .points <- points
  .palette = palette
  .steps <- steps
  .labels <- labels
  .strate <- strate
  
  BREAKSVAL <- NULL
  
  # Removing missing values
  # -------------------------------------------
  .points <- .points[!is.na(.points[,.value]),]
  
  if (!is.null(.value)) {
    .BRK <- internal_getBreaks(points[, .value], breaks = breaks, steps = .steps, labels = labels)
    .BRK <- as.factor(.BRK)
    .points$BREAKSVAL = .BRK
  }
  
  if (!is.null(.strate)) {
    STRATE <- as.factor(points[,.strate])
  }
  
  # Palettes
  # ---------------------------------------------------------
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
  
  # Labels
  # ---------------------------------------------------------
  if (is.null(labels)) {
    .labels <- levels(.points$BREAKSVAL)
  }
  
  
  # Theme base
  # ----------------------------------------------------------
  .Theme <-   theme_bw() +
    theme(panel.border = element_blank()) +
    theme(legend.key = element_blank()) +
    theme(plot.title = element_text(hjust=0.5),
          plot.subtitle = element_text(hjust=0.5),
          plot.caption = element_text(hjust=0))
    
    # theme(axis.text = element_blank()) +
    #    theme(axis.title = element_blank()) +
    #theme(axis.ticks = element_blank())
  # ----------------------------------------------------------
  #.data$long <- abs(.data$long)
  x = y = long = lat = group <- NULL
  if (!is.null(note)) {
    note <- gsub('(.{1,90})(\\s|$)', '\\1\n', note)
  }
  
  P <- ggplot()

  # Draw background if exists -----------------------------------------------
  if (.x$hasBGND) {
    P <- P + geom_raster(data=.raster, aes(x, y), fill=.raster$rgb)
  }
  
  P <- P + 
    geom_polygon(data=.data, aes(x=long, y=lat,  group=group),
                 fill=NA, color="black", size = 0.5) +
    xlab(paste("\n\n", note, sep="")) + ylab("")
  
  
  if (!is.null(.value)) {
    # Colored dots from breaks
    # ----------------------------------------------------------
    P <- P + geom_point(data=.points,
                        aes(x=longitude, y=latitude,
                            color=factor(BREAKSVAL)), 
                        size=size, alpha=0.8) +
      geom_point(data=.points,
                 aes(x=longitude, y=latitude), 
                 size=size, alpha=0.8, shape=21) +
      
      scale_color_manual(legend, values = .palette, 
                         limits=levels(.BRK),
                         labels=.labels,
                         guide = guide_legend(reverse = T)) +
      labs(title = .titles,
           subtitle = .subtitle,
           caption = .caption,
           fill = "") + 
      .Theme + coord_quickmap();
    return(P)
  }
  else {
    longitude = latitude <- NULL
    # No Stratification -------------------------------------------------------
    if (is.null(strate)) {
      P <- P + geom_point(data=points, aes(x=longitude, y=latitude), size=4, color=.pcolor, shape=16) +
        labs(title = .titles,
             subtitle = .subtitle,
             caption = .caption,
             fill = "") + 
        .Theme + coord_quickmap();
      return(P)
    }

    # Stratification ----------------------------------------------------------
    else {
      message("Using stratification\n")
      P <- P + geom_point(data=points, aes(x=longitude, y=latitude, shape=STRATE), size=4, color=.pcolor) +
        labs(title = .titles,
             subtitle = .subtitle,
             caption = .caption,
             fill = "") + 
        
        scale_shape_manual(values = c(15:18,65:75)) +
        .Theme + coord_quickmap();
      return(P)
    }
  }
}  

