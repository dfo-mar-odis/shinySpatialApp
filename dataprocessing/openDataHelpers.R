renv::install("ckanr") #you need to install the packages every time since it is a fresh container

library(ckanr)
library(sf)
source(here::here("app/R/helpers.R"))
ckanr_setup(url="https://open.canada.ca/data")



# --------------get opendata rr-----------------
# Function to retrieve opendata record and return an rr object.
#
# Inputs:
# 1. pkgId: Id string of the open data package to retrieve, used to rab metadata and text
# 2. resId: Id string of the specific open data resource containing the spatial data
# 3. Additional parameters passed to get_opendata_sf
# 4. checkDate: optional parameter to check if opendata resource has been updated
#
# Outputs:
# 1 .out_rr: output rr object containing spatial data and metadata
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
                 "attribute" = "NONE",
                 "data_sf" = data_sf,
                 "metadata" = list("contact" = contactInfo,
                                   "url" = url,
                                   "accessedOnStr" = accessedDate,
                                   "accessedDate" = today(),
                                   "constraints" = constraints,
                                   "securityLevel" = securityLevel
                   
                 ) # end metadata
             ) # end rr
  return(out_rr)
}


# --------------get opendata sf-----------------
# Function to retrieve opendata spatial data and return an sf object.
#
# Inputs:
# 1. resId: Id string of the specific open data resource containing the spatial data
# 2. Additional parameters passed to get_opendata_sf, typically from get_opendata_rr
#
# Outputs:
# 1 .out_sf: output sf object containing spatial data
get_opendata_sf <- function(resId, ...) {
  res <- resource_show(resId)
  out_sf <- download_extract_validate_sf(res$url, ...)
  return(out_sf)
}


# --------------download_extract_validate_sf-----------------
# Function that downloads a zip file, extracts it, loads an sf and cleans the sf
#
# Inputs:
# 1. zipUrl: Id string of the specific open data resource containing the spatial data
# 2. region_sf: Sf object to crop the output sf too
# 2. Additional parameters: used to specify spatial data file type
#
# Outputs:
# 1 .out_sf: output sf object containing spatial data
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
    out_sf <- st_read(gdbDir, stringsAsFactors = FALSE, layer = gdbLayer)
    out_sf$geometry <- st_geometry(out_sf)
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


# --------------download_extract_res_file-----------------
# Function that downloads a zip file of csvs, extracts it and reads the csv.
#
# Inputs:
# 1. resId: Id string of the specific open data resource containing the data
# 2. csvFileList: names of the csv file to extract from the zip file
#
# Outputs:
# 1  dfList: list of dfs, one for each csv file in csvFileList
download_extract_res_files <- function(resId, csvFileList = NULL) {
  res <- resource_show(resId)
  zipUrl <- res$url
  tempDir <- here::here("dataprocessing/temp")
  
  
  if (res$format == "CSV") {
    temp <- here::here("dataprocessing/temp/csvFile.csv")
    download.file(zipUrl, temp)
    dfList <- read.csv(temp)
    
  } else {
    
    temp <- here::here("dataprocessing/temp/temp.zip")
    
    download.file(zipUrl, temp)
    utils::unzip(temp, exdir = tempDir)
    
    outFiles <-lapply(csvFileList, function(x, dir) {
      list.files(dir, recursive=TRUE, pattern = x, 
                 include.dirs = TRUE, full.names = TRUE)
    }, dir=tempDir)
    
    dfList <- lapply(outFiles, read.csv)
    
  }
  

  # cleanup
  tempFiles <- list.files(tempDir, include.dirs = T, full.names = T, recursive = T)
  unlink(tempFiles, recursive = TRUE) 
  
  return(dfList)
}



# --------------get_check_date-----------------
# Helper function that extracts the date of an rr object in the environment.
#
# Input:
# 1. varName: string name of an rr object loaded in the global environment
#
# Outputs:
# 1 checkDate: date the rr object was accessed, or NULL if the object was not found
get_check_date <- function(varName) {
  checkDate <- NULL
  if (varName %in% ls(globalenv())) {
    var_rr <- get(varName)
    checkDate <- var_rr$accessedDate
  }
  return(checkDate)
}


# --------------lang_list-----------------
# Helper function that converts a string into a bilinugual list
lang_list <- function(inValue) {
  return(list("en" = inValue, "fr" = inValue))
}

# --------------email_format-----------------
# Helper function that converts a string email address into a linked one for use in rmd
email_format <- function(emailStr) {
  return(paste("[", emailStr, "](mailto:", emailStr, "){.email}", sep=""))
}


# --------------save_open_data-----------------
# Wrapper function to peform all the steps required to save an open data object.
save_open_data <- function(pkgId, resId, variableName, qualityTier, savePath,
                           disableCheckDate = TRUE, contactEmail = NULL, searchYears=NULL,
                           reference = NULL, ...) {
  dataSaved <- FALSE
  fnCheckDate <- NULL
  if (!disableCheckDate){
    fnCheckDate <-  get_check_date(variableName)  
  }
  temp_rr <- get_opendata_rr(pkgId, resId, checkDate = fnCheckDate, ...)
  if(!is.null(temp_rr)) {
    if (!is.null(contactEmail)){
      temp_rr$metadata$contact = contactEmail
    }
    if (!is.null(reference)){
      temp_rr$metadata$reference = reference
    }
    if (!is.null(searchYears)){
      temp_rr$metadata$searchYears = searchYears
    }
    temp_rr$metadata$qualityTier <- qualityTier
    assign(variableName, temp_rr)
    save(list = variableName, file = file.path(savePath, paste("Open/", variableName, ".RData", sep="")))
    dataSaved <- TRUE
  }
  return(dataSaved)
}


# --------------RV_to_sf-----------------
# Converts the three types of RV data files into a single coherent sf object.
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
  
  out_sf <- sf::st_as_sf(gsinf, coords = c("SLONG", "SLAT"), crs = 4326)
  
  out_sf <- left_join(out_sf, gscat, by = "MISSION_SET")
  out_sf <- left_join(out_sf, gsspec, by = "CODE")
  
  out_sf <- out_sf %>% dplyr::select("YEAR", "CODE", "Scientific Name", "Common Name", "TOTNO", "ELAT", "ELONG")
}

# ---------------GIT ACTION FUNCTIONS----------------

check_for_open_data_updates <- function() {
  openDataData <- read.csv(here::here("dataprocessing/openDataData.csv"))
  
  openDataData$checkDate <- as.numeric(strftime(
    strptime(openDataData$Accessed.Date, "%Y-%m-%d"), "%Y%m%d"))
  openDataData$pkgDate <- lapply(openDataData$Package.Id, date_from_pkg)
  
  if (any(openDataData$pkgDate > openDataData$checkDate)) {
    updatePkgs <- filter(openDataData, pkgDate > checkDate)
    updatePkgs$msg <- paste("Update", updatePkgs$rr.Name, 
                            "object from opendata record:", updatePkgs$Title, 
                            "  \n\n")
    warning(updatePkgs$msg)
  }
}

# create the open data data csv file
gen_checkdate_csv <- function() {
  dataSetDf <- data.frame("rrStr" = c("ebsa_rr", "crithab_rr", "sardist_rr", 
                                      "nbw_rr", "blueWhaleHab_rr", "finWhale_rr", 
                                      "rv_rr", "pasBay_rr"))
  load_rdata(dataSetDf$rrStr,  "MAR")
  dataSetDf$rr <- lapply(dataSetDf$rrStr, get) 
  dataSetDf$pkgId <- lapply(dataSetDf$rr, pkg_id_from_url)
  dataSetDf$AccessedDate <- lapply(lapply(dataSetDf$rr, "[[", "metadata"),
                                   "[[", "accessedDate")
  dataSetDf$title <- lapply(lapply(dataSetDf$rr, "[[", "title"),
                            "[[", "en")
  csvDf <- dplyr::select(dataSetDf, c("title", "rrStr", "pkgId", "AccessedDate"))
  csvDf$AccessedDate <- lapply(csvDf$AccessedDate, strftime, "%Y-%m-%d")
  names(csvDf) <- c("Title", "rr Name", "Package Id", "Accessed Date")
  csvDf <- apply(csvDf, 2, as.character)
  write.csv(csvDf, here::here("dataprocessing/openDataData.csv"), row.names = FALSE)
}

# helper function to extract package ids from open data rr objects
pkg_id_from_url <- function(rr) {
  if ("url" %in% names(rr$metadata)) {
    pkgId <- sub("<https://open.canada.ca/data/en/dataset/", "", rr$metadata$url$en)
    pkgId <- sub(">", "", pkgId)
    return(pkgId)
  }
}


# get update date from package
date_from_pkg <- function(pkgId) {
  opendataPKG <- package_show(pkgId)
  pkgTime <- NULL
  if ("date_modified" %in% names(opendataPKG)) {
    pkgTime <- strptime(opendataPKG$date_modified, "%Y-%m-%d %H:%M:%S")  
    pkgTime <- as.numeric(strftime(pkgTime, "%Y%m%d"))
  } else if ("metadata_modified" %in% names(opendataPKG)) {
    pkgTime <- strptime(opendataPKG$metadata_modified, "%Y-%m-%dT%H:%M:%S")  
    pkgTime <- as.numeric(strftime(pkgTime, "%Y%m%d"))
    
  }
  return(pkgTime)
}


# -------------GIT ACTION FUNCTIONS ----------------
# create the open data data csv file
gen_checkdate_csv <- function() {
  dataSetDf <- data.frame("rrStr" = c("ebsa_rr", "crithab_rr", "sardist_rr", 
                                      "nbw_rr", "blueWhaleHab_rr", "finWhale_rr", 
                                      "rv_rr", "pasBay_rr"))
  load_rdata(dataSetDf$rrStr,  "MAR")
  dataSetDf$rr <- lapply(dataSetDf$rrStr, get) 
  dataSetDf$pkgId <- lapply(dataSetDf$rr, pkg_id_from_url)
  dataSetDf$AccessedDate <- lapply(lapply(dataSetDf$rr, "[[", "metadata"),
                                   "[[", "accessedDate")
  dataSetDf$title <- lapply(lapply(dataSetDf$rr, "[[", "title"),
                            "[[", "en")
  csvDf <- dplyr::select(dataSetDf, c("title", "rrStr", "pkgId", "AccessedDate"))
  csvDf$AccessedDate <- lapply(csvDf$AccessedDate, strftime, "%Y-%m-%d")
  names(csvDf) <- c("Title", "rr Name", "Package Id", "Accessed Date")
  csvDf <- apply(csvDf, 2, as.character)
  write.csv(csvDf, here::here("dataprocessing/openDataData.csv"), row.names = FALSE)
}

# helper function to extract package ids from open data rr objects
pkg_id_from_url <- function(rr) {
  if ("url" %in% names(rr$metadata)) {
    pkgId <- sub("<https://open.canada.ca/data/en/dataset/", "", rr$metadata$url$en)
    pkgId <- sub(">", "", pkgId)
    return(pkgId)
  }
}
