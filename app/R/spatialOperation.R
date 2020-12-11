ls_oper <- c("intersects", "contains", "covers", "within", "extract")
ls_oper_fun <- 

spatialOperation <- function(slc, x = NULL, y = NULL) {
  
  FUN <- switch(slc,
    "intersects" = sf::st_intersects, 
    "contains" = sf::st_contains, 
    "covers" = sf::st_covers, 
    "within" = sf::st_within, 
    "extract" = raster::extract
  )
  
  FUN
}