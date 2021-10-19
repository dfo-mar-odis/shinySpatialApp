renv::install("ckanr") #you need to install the packages every time since it is a fresh container

library(ckanr)
library(sf)
source(here::here("app/R/helpers.R"))
ckanr_setup(url="https://open.canada.ca/data")


get_opendata_rr <- function(pkgId, resId, region_sf=NULL, gdbLayer=NULL, checkDate=NULL) {
  opendataPKG <- package_show(pkgId)
  
  # check if package has been updated since checkdate
  if (!is.null(checkDate)){
    if ("date_modified" %in% names(opendataPKG)) {
      pkgTime <- strptime(opendataPKG$date_modified, "%Y-%m-%d %H:%M:%S")  
      if (pkgTime < checkDate) {
        return(NULL)
      }
    } else if ("metadata_modified" %in% names(opendataPKG)) {
      pkgTime <- strptime(opendataPKG$metadata_modified, "%Y-%m-%dT%H:%M:%S")  
      if (pkgTime < checkDate) {
        return(NULL)
      }
    }
  }
  
  pkgTitle <- opendataPKG$title_translated
  contactInfo <- opendataPKG$metadata_contact
  pkgText <- opendataPKG$notes_translated
  
  data_sf <- get_opendata_sf(resId, region_sf, gdbLayer=gdbLayer)
  
  url <- list("en" = paste("<https://open.canada.ca/data/en/dataset/", pkgId, ">", sep =""), 
              "fr" = paste("<https://open.canada.ca/data/fr/dataset/", pkgId, ">", sep =""))  
  securityLevel <- list("en" = "None", "fr"= "Aucun")
  constraints <- list("en" = "None", "fr"= "Aucun")
  accessedDate <- list("en" = NA, "fr" = NA)
  # date nonesense:
  Sys.setlocale(locale = "French")
  accessedDate$fr <-paste(strftime(today(), "%B %d, %Y"), "sur le Portail de données ouvertes", url$fr)
  Sys.setlocale(locale = "English")
  accessedDate$en <- paste(strftime(today(), "%B %d, %Y"), "from Open Data", url$en)
  
  out_rr <- list("title" = pkgTitle,
                 "text" = pkgText,
                 "url" = url,
                 "attribute" = "NONE",
                 "accessedOnStr" = accessedDate,
                 "accessedDate" = today(),
                 "contact" = contactInfo,
                 "constraints" = constraints,
                 "securityLevel" = securityLevel,
                 "data_sf" = data_sf)
  return(out_rr)
}


get_opendata_sf <- function(resId, ...) {
  res <- resource_show(resId)
  out_sf <- download_extract_validate_sf(res$url, ...)
  return(out_sf)
}

download_extract_validate_sf <- function(zipUrl, region_sf=NULL, gdbLayer=NULL) {
  tempDir <- here::here("dataprocessing/temp")
  temp <- here::here("dataprocessing/temp/temp.zip")
  
  download.file(zipUrl, temp)
  utils::unzip(temp, exdir = tempDir)
  # if there's a shape file read that:
  shpFile <- list.files(tempDir, recursive=TRUE, pattern="\\.shp$", full.names = TRUE)
  gdbDir <- list.files(tempDir, recursive=TRUE, pattern="\\.gdb$", 
                       include.dirs	= TRUE, full.names = TRUE)
    
  if (length(shpFile) > 0) {
    out_sf <- st_read(shpFile, stringsAsFactors = FALSE)
  } else if (length(gdbDir) > 0) {
    out_gdb <- st_read(gdbDir, stringsAsFactors = FALSE, layer = gdbLayer)
    out_sf <- out_gdb
    out_sf$geometry <- st_geometry(out_gdb)
  }
  
  if  (inherits(sf::st_geometry(out_sf), "sfc_GEOMETRY")) {
    out_sf <- st_cast(out_sf, "MULTIPOLYGON")
  }
  out_sf <- st_make_valid(out_sf)
  
  if (!is.null(region_sf)) {
    newRegion_sf <- st_transform(region_sf, crs = sf::st_crs(out_sf))
    out_sf <- sf::st_crop(out_sf, newRegion_sf)
  }
  
  out_sf <- st_transform(out_sf, crs = 4326)
  
  
  # cleanup
  tempFiles <- list.files(tempDir, include.dirs = T, full.names = T, recursive = T)
  unlink(tempFiles, recursive = TRUE) 
  
  return(out_sf)
}

get_check_date <- function(varName) {
  checkDate <- NULL
  if (varName %in% ls(globalenv())) {
    var_rr <- get(varName)
    checkDate <- var_rr$accessedDate
  }
  return(checkDate)
}
