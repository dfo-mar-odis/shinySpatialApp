readSpatial <- function(con, crs = 3857) { 
  # rds 
  if (grepl("\\.[Rr]ds", con)) {
    out <- readRDS(con)
  } else {
    # msgInfo("reading", con)
    out <- read_sf_try(con, crs = crs)
    if (isFALSE(out)) {
      out <- read_raster_try(con, crs = crs)
    }
    # 
    if (isFALSE(out)) {
      msgError("cannot read", con)
      out <- NULL 
    } 
  }
  out
}
  

read_sf_try <- function(con, crs = 3857) {
  out <- tryCatch(sf::st_read(con, quiet = TRUE), error = function(x) FALSE)
  if ("sf" %in% class(out) & identical(sf::st_crs(out), sf::st_crs(crs))) {
    out <- sf::st_transform(out, crs = crs)
  }
  out 
}

read_raster_try <- function(con, crs = 3857) {
  out <- tryCatch(raste::raster(con), error = function(x) FALSE)
  if (grepl("raster", class(out))) {   
    out <- raster::projectRaster(out, sf::st_crs(crs)$proj4string)
  }
  out 
}
