## ---------------------------------------------------------------------------
## Method : classDots
## Output : A map with dots wich are stratified
## ---------------------------------------------------------------------------
classDots <- function(x, 
                      data,
                      color="red",
                      value = NULL,
                      breaks = NULL,
                      steps = 5,
                      labels = NULL,
                      opacity = 0.5,
                      title="",
                      legend = NULL) UseMethod("classDots", x)

classDots.GADMWrapper <- function(
  x,
  data,
  color="red",
  value = NULL,
  breaks = NULL,
  steps = 3,
  labels = NULL,
  opacity = 0.5,
  title="",
  legend = NULL) {
  
  # Détermination du champs d'extraction des shapefiles
  # ----------------------------------------------------------
  if (x$level == 0) {
    .name <-"ISO"
  } else {
    .name <- sprintf("NAME_%d", x$level)
  }
  
  # Variables locales
  # ----------------------------------------------------------
  .map    <- fortify(x$spdf, region=.name);
  .title  <- title
  .pcolor <- color
  .value  <- value
  .breaks <- breaks
  .points <- data
  .steps  <- steps
  .labels <- labels
  .legend <- legend
  .opacity <- opacity
  .sizeIndex <- length(.breaks) - 1
 
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
  
  .BRK <- computeBreaks(.points[, .value], breaks = breaks, steps = .steps, labels = labels)
  .BRK <- as.factor(.BRK)
  .points <- .points %>% mutate(PSIZE = factor(as.integer(.BRK)))

  # Labels
  # ---------------------------------------------------------
  if (is.null(labels)) {
    .labels <- levels(.BRK)
  }
  
  # Tracé de la carte administrative
  # ----------------------------------------------------------
  long = lat = group <- NULL
  P <- ggplot() +
    geom_polygon(data=.map, aes(x=long, y=lat,  group=group),
                 fill="#efefef", color="black", size = 0.5)
  
  # Plot des points sur la carte
  # ----------------------------------------------------------
  P <- P + geom_point(data=.points,
                      aes(x=longitude, y=latitude, size = PSIZE),
                      color = "#000000", shape=21, fill = .pcolor,
                      alpha = .opacity)
  SCALE_VALUES = c("1"=5, "2"=10, "3"=20, "4"=30, "5"=45)
  P <- P + scale_size_manual(name = .legend,
                             values = SCALE_VALUES,
                             guide = guide_legend(reverse = T),
                             limits = as.character(c(1:.sizeIndex)),
                             labels=.labels) +
  labs(title = title) 
  
  # Réglage du theme
  # ---------------------------------------------------------
  P + theme_bw() +
    theme(plot.title = element_text(size=20)) +
    theme(legend.text=element_text(size=14)) +
    theme(legend.title=element_text(size=16)) +
    theme(panel.border = element_blank()) +
    theme(legend.key = element_blank()) +
    theme(axis.text = element_blank()) +
    theme(axis.title = element_blank()) +
    theme(axis.ticks = element_blank()) +
    coord_map();
  
}
