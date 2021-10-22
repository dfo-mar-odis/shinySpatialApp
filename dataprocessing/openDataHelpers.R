renv::install("ckanr") #you need to install the packages every time since it is a fresh container

library(ckanr)
library(sf)
source(here::here("app/R/helpers.R"))
ckanr_setup(url="https://open.canada.ca/data")


get_opendata_rr <- function(pkgId, resId, region_sf=NULL, gdbLayer=NULL, tifFile=NULL, checkDate=NULL) {
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
  if (!is.null(resId)) {
    data_sf <- get_opendata_sf(resId, region_sf, gdbLayer=gdbLayer, tifFile=tifFile)  
  } else {
    data_sf <- NULL
  }
  
  
  url <- list("en" = paste("<https://open.canada.ca/data/en/dataset/", pkgId, ">", sep =""), 
              "fr" = paste("<https://open.canada.ca/data/fr/dataset/", pkgId, ">", sep =""))  
  securityLevel <- list("en" = "None", "fr"= "Aucun")
  constraints <- list("en" = "None", "fr"= "Aucun")
  accessedDate <- list("en" = NA, "fr" = NA)
  # date nonesense:
  Sys.setlocale(locale = "French")
  accessedDate$fr <-paste(strftime(today(), "%B %d, %Y"), "sur le Portail de données ouvertes")
  Sys.setlocale(locale = "English")
  accessedDate$en <- paste(strftime(today(), "%B %d, %Y"), "from Open Data")
  
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

download_extract_validate_sf <- function(zipUrl, region_sf = NULL, gdbLayer = NULL, tifFile = NULL) {
  tempDir <- here::here("dataprocessing/temp")
  temp <- here::here("dataprocessing/temp/temp.zip")
  
  download.file(zipUrl, temp)
  utils::unzip(temp, exdir = tempDir)
  # if there's a shape file read that:
  shpFile <- list.files(tempDir, recursive=TRUE, pattern="\\.shp$", full.names = TRUE)
  gdbDir <- list.files(tempDir, recursive=TRUE, pattern="\\.gdb$", 
                       include.dirs	= TRUE, full.names = TRUE)
  tifRasterFile <- list.files(tempDir, recursive=TRUE, pattern = paste("\\",tifFile, "$", sep = ""), 
                        include.dirs	= TRUE, full.names = TRUE) 
  if (length(shpFile) > 0) {
    out_sf <- st_read(shpFile, stringsAsFactors = FALSE)
  } else if (length(gdbDir) > 0) {
    out_gdb <- st_read(gdbDir, stringsAsFactors = FALSE, layer = gdbLayer)
    out_sf <- out_gdb
    out_sf$geometry <- st_geometry(out_gdb)
  } else if (length(tifRasterFile) > 0) {
    tifRaster <- raster(tifRasterFile)
    out_sf <- stars::st_as_stars(tifRaster) %>% sf::st_as_sf()
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

download_extract_res_files <- function(resId, csvFileList = NULL) {
  res <- resource_show(resId)
  zipUrl <- res$url
  
  tempDir <- here::here("dataprocessing/temp")
  temp <- here::here("dataprocessing/temp/temp.zip")
  
  download.file(zipUrl, temp)
  utils::unzip(temp, exdir = tempDir)

  outFiles <-lapply(csvFileList, function(x, dir) {
    list.files(dir, recursive=TRUE, pattern = x, 
               include.dirs = TRUE, full.names = TRUE)
    }, dir=tempDir)
  
  dfList <- lapply(outFiles, read.csv)
  
  # cleanup
  tempFiles <- list.files(tempDir, include.dirs = T, full.names = T, recursive = T)
  unlink(tempFiles, recursive = TRUE) 
  
  return(dfList)
}


get_check_date <- function(varName) {
  checkDate <- NULL
  if (varName %in% ls(globalenv())) {
    var_rr <- get(varName)
    checkDate <- var_rr$accessedDate
  }
  return(checkDate)
}


lang_list <- function(inValue) {
  return(list("en" = inValue, "fr" = inValue))
}

email_format <- function(emailStr) {
  return(paste("[", emailStr, "](mailto:", emailStr, "){.email}", sep=""))
}


save_open_data <- function(pkgId, resId, variableName, qualityTier, savePath,
                           disableCheckDate = TRUE, contactEmail = NULL, searchYears=NULL, ...) {
  dataSaved <- FALSE
  fnCheckDate <- NULL
  if (!disableCheckDate){
    fnCheckDate <-  get_check_date(variableName)  
  }
  temp_rr <- get_opendata_rr(pkgId, resId, checkDate = fnCheckDate, ...)
  if(!is.null(temp_rr)) {
    if (!is.null(contactEmail)){
      temp_rr$contact = contactEmail
    }
    if (!is.null(searchYears)){
      temp_rr$searchYears = searchYears
    }
    temp_rr$qualityTier <- qualityTier
    assign(variableName, temp_rr)
    save(list = variableName, file = file.path(savePath, paste("Open/", variableName, ".RData", sep="")))
    dataSaved <- TRUE
  }
  return(dataSaved)
}



RV_to_sf <- function(gscat, gsinf, gsspec, minYear){
  gscat <- gscat %>% tidyr::unite("MISSION_SET", MISSION:SETNO, remove = TRUE)
  gscat <- gscat %>% rename(CODE = SPEC)
  
  gsinf <- gsinf %>% tidyr::unite("MISSION_SET", MISSION:SETNO, remove = FALSE)
  gsinf$YEAR <- lubridate::year(gsinf$SDATE)
  gsinf <- gsinf %>% dplyr::filter(YEAR >= minYear)
  
  gsspec <- dplyr::distinct(gsspec)
  gsspec <- gsspec %>% transmute(gsspec, SPEC = stringr::str_to_sentence(SPEC))
  gsspec <- gsspec %>% transmute(gsspec, COMM = stringr::str_to_sentence(COMM))
  gsspec <- gsspec %>% rename("Common Name"= COMM, "Scientific Name" = SPEC)
  
  if (nrow(gsinf) > 0){
    out_sf <- sf::st_as_sf(gsinf, coords = c("SLONG", "SLAT"), crs = 4326)
  } else {
    out_sf <- gsinf
  }
  
  out_sf <- left_join(out_sf, gscat, by = "MISSION_SET")
  out_sf <- left_join(out_sf, gsspec, by = "CODE")
  
  out_sf <- out_sf %>% dplyr::select("YEAR", "CODE", "Scientific Name", "Common Name", "TOTNO", "ELAT", "ELONG")
}



