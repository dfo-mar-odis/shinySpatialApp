renv::install("ckanr") #you need to install the packages every time since it is a fresh container

library(ckanr)
library(sf)
source(here::here("app/R/helpers.R"))
ckanr_setup(url="https://open.canada.ca/data")


get_opendata_rr <- function(pkgId, resId, gdbLayer=NULL) {
  opendataPKG <- package_show(pkgId)
  pkgTitle <- opendataPKG$title_translated
  contactInfo <- opendataPKG$metadata_contact
  
  data_sf <- get_opendata_sf(resId, gdbLayer=gdbLayer)
  
  # date nonesense:
  accessedDate <- list("en"=NA, "fr"=NA)
  Sys.setlocale(locale = "French")
  accessedDate$fr <- strftime(today(), "%B %d, %Y")
  Sys.setlocale(locale = "English")
  accessedDate$en <- strftime(today(), "%B %d, %Y")
  
  out_rr <- list("title" = pkgTitle,
                 "attribute" = "NONE",
                 "accessedOn" = accessedDate,
                 "contact" = contactInfo,
                 "data_sf" = data_sf)
  return(out_rr)
}


get_opendata_sf <- function(resId, ...) {
  res <- resource_show(resId)
  out_sf <- download_extract_validate_sf(res$url, ...)
  return(out_sf)
}

download_extract_validate_sf <- function(zipUrl, gdbLayer=NULL) {
  tempDir <- here::here("dataprocessing/temp")
  temp <- here::here("dataprocessing/temp/temp.zip")
  
  download.file(zipUrl, temp)
  utils::unzip(temp, exdir = tempDir)
  # if there's a shape file read that:
  shpFile <- list.files(tempDir, recursive=TRUE, pattern="\\.shp$", full.names = TRUE)
  gdbDir <- list.files(tempDir, recursive=FALSE, pattern="\\.gdb$", full.names = TRUE)
    
  if (length(shpFile) > 0) {
    out_sf <- st_read(shpFile, stringsAsFactors = FALSE)
  } else if (length(gdbDir) > 0) {
    out_gdb <- st_read(gdbDir, stringsAsFactors = FALSE, layer = gdbLayer)
    out_sf <- out_gdb
    out_sf$geometry <- st_geometry(out_gdb)
  }

  out_sf <- st_transform(out_sf, crs = 4326)
  
  if  (inherits(sf::st_geometry(out_sf), "sfc_GEOMETRY")) {
    out_sf <- st_cast(out_sf, "MULTIPOLYGON")
  }
  out_sf <- st_make_valid(out_sf)
  # as.data.frame(table(st_geometry_type(test_sf)))
  # cleanup
  tempFiles <- list.files(tempDir, include.dirs = T, full.names = T, recursive = T)
  unlink(tempFiles, recursive = TRUE) 
  
  return(out_sf)
}
