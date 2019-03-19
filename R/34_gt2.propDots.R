propDots.gadm_sf <- function(x, 
                         data, 
                         value, 
                         breaks=NULL, 
                         range=NULL, 
                         labels=NULL, 
                         color="red", 
                         title="", 
                         subtitle = "",
                         caption  = "",
                         note=NULL) {

  .x <- x
  
  # Select a name to fortify it (for ggplot2) -------------------------------
  .name <- gadm_getLevelName(x)
  
  if (.x$hasBGND == TRUE) {
    .raster <- x$BGND
  }
  
  .data <- data
  .value <- value
  .title <- title
  .pcolor <- color
  
  .map <- x$sf
  
  getBreaks <- function(value) {
    .min = min(data[,value], na.rm = T)
    .max = max(data[,value], na.rm = T)
    .r = .max - .min
    .B <- round(c(.r * 0.2, .r * 0.4, .r * 0.6, .r * 0.8, .r * 1.0), 0)
    list(.B, .min, .max)
  }
  
  .data <- .data[order(-.data[,.value]),]
  .inter <- getBreaks(value)
  .breaks <- breaks
  
  if (is.null(breaks)) {
    .breaks <- .inter[[1]]
  }
  
  .range <- range
  if (is.null(range)) {
    .range <- c(.inter[[2]], .inter[[3]])
  }
  
  .labels = labels
  if (is.null(labels)) {
    .labels <- .breaks
  }
  
  long = lat = group <- NULL
  if (!is.null(note)) {
    note <- gsub('(.{1,120})(\\s|$)', '\\1\n', note)
  }
  
  x = y = NULL
  
  P <- ggplot()
  
  # Draw background if exists -----------------------------------------------
  if (.x$hasBGND) {
    P <- P + geom_raster(data=.raster, aes(x, y), fill=.raster$rgb)
  }
  
  
  # Draw the shapefile ------------------------------------------------------
  P <- P + geom_sf(data=.map, fill=NA, size = 0.5) +
    geom_point(data=.data,
               aes_string(x="longitude", y="latitude", 
                          size=eval(value)), 
               fill=.pcolor, color="#000000", shape=21, alpha=0.25) +
    xlab("") + ylab("") +
    scale_size_area(max_size = 10, breaks=.breaks, limits = .range, labels=.labels) +
    labs(title = .title,
         subtitle = subtitle,
         caption  = paste(caption, note, sep="\n\n"), 
         fill = "") + 
    theme_bw() +
    
    theme(plot.title = element_text(hjust=0.5),
          plot.subtitle = element_text(hjust=0.5),
          plot.caption = element_text(hjust=0))+
    
    theme(panel.border = element_blank(),
          plot.margin = margin(0, 0.1, 0, 0.1, "cm")) +
    theme(legend.key = element_blank()) +
    theme(axis.text = element_blank()) +
#    theme(axis.title = element_blank()) +
    coord_sf();
  P
}
