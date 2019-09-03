#' creates multivariate dot-density maps
#'
#' @param map 
#' @param data 
#' @param adm.join 
#' @param variables 
#' @param cases.by.dots 
#' @param palette 
#' @param title 
#' @param subtitle 
#' @param caption 
#'
#' @return
#' @export
#'
#' @examples
dotDensity <- function(map,
                            data,
                            adm.join  = NULL,
                            values    = NULL,
                            cases.by.dots = 100,
                            dot.size  = .25,
                            labels    = NULL,
                            palette   = NULL,
                            title     = NULL,
                            subtitle  = NULL,
                            caption   = NULL)
  
{

  # generate_samples --------------------------------------------------------
  # -------------------------------------------------------------------------
  generate_samples <- function(data) {
    
    .df <- sf::st_drop_geometry(data)
    .G <- st_geometry(data)
    .Size <- .df$VALUE
    .i <- 1
    .G <- lapply(.G, function(L){
      pts <- suppressMessages(st_sample(L, size = .Size[.i], exact = TRUE))
      .mp <- st_multipoint(matrix(unlist(pts), ncol=2, byrow=TRUE))
      .i <<- .i + 1
      .mp
    })
    .G <- st_sfc(.G, crs=4326)
    .sf <- sf::st_set_geometry(.df, .G)
    .sf
  }
  
  .map <- map$sf
  .sf_data <- data
  .palette <- palette
  .cbd <- cases.by.dots
  .vars <- values
  .labels <- labels
  .title <- title
  .subtitle <- subtitle
  .caption <- caption
    
  if (is.null(.labels)) {
    .labels <- .vars
  }

  # Protection against NULL sample
  .cbd <- min( min(data[, values]), .cbd)
  
  # Bindings ----------------------------------------------------------------
  # -------------------------------------------------------------------------
  S_VAR <- VALUE <- NULL
  
  # Join map and data -------------------------------------------------------
  # -------------------------------------------------------------------------
  .levelName <- sprintf("NAME_%d", map$level)
  names(.sf_data)[names(.sf_data)==adm.join] <- .levelName
  .sf_data <- dplyr::left_join(.sf_data, .map) %>%
    tidyr::drop_na()
#   .sf_data <- .sf_data[complete.cases(.sf_data), ]

  # data frame of number of dots to plot for each var -----------------------
  # -------------------------------------------------------------------------
  .num_dots <- as.data.frame(.sf_data) %>% dplyr::select(.vars) %>% 
    dplyr::mutate_all(.funs = list(~round((./.cbd)))) %>%
    dplyr::mutate(geometry = .sf_data$geometry) %>%
    tidyr::gather("S_VAR", "VALUE", .vars) %>%
    dplyr::select(S_VAR, VALUE, geometry) %>%
    st_sf()
  

  # generate samples --------------------------------------------------------
  # -------------------------------------------------------------------------
  .Points <- generate_samples(.num_dots)

  # Plot map + dots ---------------------------------------------------------
  # -------------------------------------------------------------------------
  .legend <- sprintf("1 dot = %d cases", .cbd)
  
  .P <- ggplot() +
    geom_sf(data = .map, colour = "black", size=0.25, fill="transparent") +
        geom_sf(data=.Points, aes(color=S_VAR, fill=S_VAR), size = dot.size, ) +
        scale_color_manual(values=.palette, guide=FALSE) +
        scale_fill_manual(values=.palette, labels = .labels) +
        labs(title = .title,
             subtitle = .subtitle,
             caption = .caption,
             fill = .legend) +
    
        theme_light() +
        coord_sf(crs = 4326, datum = NA)
  
  .P
}

