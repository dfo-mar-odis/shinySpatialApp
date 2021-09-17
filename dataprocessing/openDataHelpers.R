renv::install("ckanr") #you need to install the packages every time since it is a fresh container

library(ckanr)

ckanr_setup(url="https://open.canada.ca/data")
get_opendata_rr <- function(pkgId, resId) {
  opendataPKG <- package_show(pkgId)
  pkgTitle <- opendataPKG$title_translated
  contactInfo <- opendataPKG$metadata_contact
  
  data_sf <- get_opendata_sf(resId)
  
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


get_opendata_sf <- function(resId) {
  res <- resource_show(resId)
  outSf <- download_extract_validate_sf(res$url)
  return(outSf)
}

download_extract_validate_sf <- function(zipUrl) {
  tempDir <- here::here("dataprocessing/temp")
  temp <- here::here("dataprocessing/temp/temp.zip")
  
  download.file(zipUrl, temp)
  utils::unzip(temp, exdir =tempDir)
  shpFile <- list.files(tempDir, recursive=TRUE, pattern="\\.shp$", full.names = TRUE)
  
  out_sf <- st_read(shpFile, stringsAsFactors = FALSE)
  out_sf <- st_transform(out_sf, crs = 4326)
  out_sf <- st_make_valid(out_sf)
  
  # cleanup
  tempFiles <- list.files(tempDir, include.dirs = T, full.names = T, recursive = T)
  unlink(tempFiles, recursive = TRUE) 
  
  return(out_sf)
}
