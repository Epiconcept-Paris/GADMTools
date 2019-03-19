
# inner_stats_matrixStandardizeWeights <- function(M) {
#   M / apply(M, MARGIN = 1, FUN = function(X) (sum(X)))
# }

# Compute Moran's I -------------------------------------------------------
# -------------------------------------------------------------------------
inner_stats_moran <- function(p_y, p_w) {
  .n <- length(p_y)
  
  .numerateur   <- 0.0
  .denominateur <- 0.0
  # .meanX        <- mean(y)
 
  # Center i ----------------
  .y <- scale(p_y, scale=FALSE)
  
  for(i in 1:.n) {
    for(j in 1:.n) {
      .Wij <- p_w[i,j]
      if (.Wij != 0){
        .numerateur <- .numerateur + .Wij * (.y[i] * .y[j])
      }
    }
    .denominateur <- .denominateur + (.y[i])^2
  }
  
  .S0 <- sum(p_w)
  
  # As matrix is normalized, n / S0 = 1
  # .num <- .n * .numerateur
  # .den <- S0 * .denominateur
  # .moran_I <- .num / .den
  .moran_I <- .numerateur / .denominateur
  .moran_I
}

# Run a permutatione test and return the p.value --------------------------
# -------------------------------------------------------------------------
inner_stats_pTest <- function(y, w, n = 499) {
  stat <- inner_stats_moran(y, w)
  sim <- replicate(n, inner_stats_moran(sample(y, length(y)), w))
  p.value <- mean((all <- c(stat, sim)) >= stat)

  return(p.value)
}
  

# Return a standardized matrixx of weights --------------------------------
# -------------------------------------------------------------------------
inner_stats_polygonsGetWeights <- function(map) {
  .W <- sf::st_intersects(map, sparse = FALSE)
  .W <- .W * 1
  diag(.W) <- 0
  # .W <- inner_stats_matrixStandardizeWeights(.W)
  .W <- .W / apply(.W, MARGIN = 1, FUN = function(X) (sum(X)))
  .W
}

# library(GADMTools)
# FRA <- gadm_sf.loadCountries("FRA", level=2, basefile="./")
# IDF <- gadm_subset(FRA, 1, regions = "Île-de-France")
# plotmap(IDF)
# W <- inner_stats_polygonsGetWeights(IDF$sf)
# W