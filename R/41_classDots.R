classDots.gadm_sf <- function( x,
                           data,
                           color="red",
                           value = NULL,
                           breaks = NULL,
                           steps = 5,
                           labels = NULL,
                           opacity = 0.5,
                           title="",
                           note=NULL,
                           legend = NULL) {
  
  loadNamespace("dplyr")
  

  # Locales variables
  # ----------------------------------------------------------
  .x <- x
  
  .map    <- x$sf
  
  .title  <- title
  .pcolor <- color
  .value  <- value
  .breaks <- breaks
  .points <- data
  .steps  <- steps
  .labels <- labels
  .legend <- legend
  .opacity <- opacity
  .sizeIndex <- 1
  
  longitude <- latitude <- PSIZE <- NULL

  if (.x$hasBGND == TRUE) {
    .raster <- x$BGND
  }
  
  # Test length of breaks
  # -------------------------------------------
  if (.sizeIndex > 5) {
    stop("Error: Too much breaks!")
  }
  
  # Removing missing values
  # -------------------------------------------
  .points <- .points[!is.na(.points[,.value]),]
  
  if (is.null(.value)) {
    stop("Error: parameter [value] absent!")
  }
  
  .BRK <- internal_getBreaks(.points[, .value], breaks = breaks, steps = .steps, labels = labels)
  .BRK <- as.factor(.BRK)
  .points <- .points %>% dplyr::mutate(PSIZE = factor(as.integer(.BRK)))

  # Labels
  # ---------------------------------------------------------
  if (is.null(labels)) {
    .labels <- levels(.BRK)
  }
  
  .sizeIndex <- length(levels(.BRK))
  
  # Plot admin map
  # ----------------------------------------------------------
  long = lat = group <- x <- y <- NULL
  
  P <- ggplot()
  # Draw background if exists -----------------------------------------------
  if (.x$hasBGND) {
    P <- P + geom_raster(data=.raster, aes(x, y), fill=.raster$rgb)
  }
  
  # Draw the shapefile ------------------------------------------------------
  P <- P + geom_sf(data=.map, fill=NA, color="black", size = 0.5)

  # Plot points on map
  # ----------------------------------------------------------
  P <- P + geom_point(data=.points,
                      aes(x=longitude, y=latitude, size = PSIZE),
                      color = .pcolor, shape=21, fill = .pcolor,
                      alpha = .opacity)
  SCALE_VALUES = c("1"=2, "2"=4, "3"=7, "4"=11, "5"=16, "6"=22)
  P <- P + scale_size_manual(name = .legend,
                             values = SCALE_VALUES,
                             guide = guide_legend(reverse = T),
                             # limits = as.character(c(1:.sizeIndex+1)),
                             limits = as.character(c(1:.sizeIndex)),
                             labels=.labels) +
    labs(title = title) 
  note <- gsub('(.{1,90})(\\s|$)', '\\1\n', note)
  P = P + xlab(paste("\n\n", note, sep="")) + ylab("")
  
  # Theme tuning
  # ---------------------------------------------------------
  P + theme_bw() +
    theme(plot.title = element_text(size=20)) +
    theme(legend.text=element_text(size=14)) +
    theme(legend.title=element_text(size=16)) +
    theme(panel.border = element_blank()) +
    theme(legend.key = element_blank()) +
    theme(axis.text = element_blank()) +
    #     theme(axis.title = element_blank()) +
    theme(axis.ticks = element_blank()) +
    coord_sf();
  
}
