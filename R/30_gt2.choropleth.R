
choropleth.GT2 <- function(x,
                           data,
                           value=NULL,
                           breaks = NULL,
                           steps = 5,
                           adm.join = NULL,
                           legend   = NULL,
                           labels   = NULL,
                           palette  = NULL,
                           title="",
                           subtitle = NULL,
                           caption  = NULL) {
  .x <- x
  .map <- as.data.frame(x$spdf)
  
  if (is.null(value)) stop("Unknown value (NULL)\n")
  
  if (is.null(adm.join)) {
    stop("adm.join is NULL! You MUST provide a name.")
  }

  if (.x$hasBGND == TRUE) {
    .raster <- x$BGND
  }
  
  .data <- data
  .value <- value
  .legend <- legend
  .palette <- palette
  .steps <- steps
  .labels <- labels
  .subtitle <- subtitle
  .caption <- caption
  .titles <- title
  
  # get breaks ----------------------------------------------------
  .data <- internal.gt2.getBreaks(.data, .value, breaks, .steps, .labels)
  

  # join by .levelName --------------------------------------------
  .levelName <- gadm.getLevelName(.map, x$level)
  names(.data)[names(.data)==adm.join] <- .levelName
  .map <- dplyr::left_join(.map, .data, by = .levelName)


#  names(P)[names(P)==value] <- "CHPLT_VALUE"
  
  
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
  
  .fill = sprintf("%s_cut", value)
  
  if (is.null(labels)) {
    .labels <- levels(.map[, .fill])
  }
  
  if (is.null(legend)) .legend <- value
  
  long <- lat <- group <- x <- y <- BREAKS <- Specialistes <- NULL
  
  
  # Plot map ----------------------------------------------------------------
  .map <- sf::st_sf(.map)
 # return(.map)
  
  .P <- ggplot()
  
  # Draw background if exists -----------------------------------------------
  if (.x$hasBGND) {
    .P <- .P + geom_raster(data=.raster, aes(x, y), fill=.raster$rgb)
  }
  
  # Draw the shapefile ------------------------------------------------------
  .P <- .P + geom_sf(data=.map, aes_string(fill=.fill), color="black", size = 0.25)
  .P <-  internal_getNorthScaleBar(.P) +
    
    scale_fill_manual(.legend, values = .palette,
                      limits=.labels,
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
    coord_sf();
  if (.legend == FALSE) {
    .P <- .P + theme(legend.position="none")
  }
  .P
}

