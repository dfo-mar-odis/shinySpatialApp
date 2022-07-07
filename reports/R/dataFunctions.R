source(here::here("config.R"))
source(here::here("reports/R/plotFunctions.R"))
library(dplyr)

# function to make sure that the data dir for this region is set up
data_dir_check <- function() {
  data_dir <- here::here("app/data/", regionStr)
  dirList <- file.path(data_dir, c("Open", "Secure", "Protected"))
  # if dir already exists, this will fail silently
  output <- lapply(dirList, dir.create, showWarnings = FALSE)
  return()
}

# -----------------copy_rdata_files-------------
# Function syncs local copy of here::here("app/data") with remote data dir
# Inputs: None
# Outputs: confirmation that files are now up to date
copy_rdata_files <- function() {
  # make sure dirs are present:
  data_dir_check()
  
  rDataDir <- dirname(remoteFileSavePath)
  # replace backslashes introduced by dirname:
  rDataDir <- gsub("\\\\", "/", rDataDir) 
  
  localDataDir <- dirname(localFileSavePath)
  
  remoteInfo <- file.info(list.files(rDataDir, recursive = TRUE, full.names = TRUE))
  remoteInfo["filenames"] <- sub(rDataDir, "", rownames(remoteInfo))
  
  localInfo <- file.info(list.files(localDataDir, recursive = TRUE, full.names = TRUE))
  localInfo["filenames"] <- sub(localDataDir, "", rownames(localInfo))
  
  missingList <- !(remoteInfo$filenames %in% localInfo$filenames) 
  if (any(missingList)) {
    missingFileList <- dplyr::filter(remoteInfo, missingList)$filenames
    missingFilePath <- rownames(dplyr::filter(remoteInfo, remoteInfo$filename %in% missingFileList))
    destList <- file.path(localDataDir, missingFileList)
    copyResults <- file.copy(missingFilePath, destList, overwrite = TRUE)
  }
  
  allInfo <- merge(remoteInfo, localInfo, by="filenames")
  updateList <- allInfo$mtime.x > allInfo$mtime.y
  if (any(updateList)) {
    updateFileList <- dplyr::filter(allInfo, updateList)$filenames
    updateFilePath <- rownames(dplyr::filter(remoteInfo, remoteInfo$filename %in% updateFileList))
    destList <- file.path(localDataDir, updateFileList)
    copyResults <- file.copy(updateFilePath, destList, overwrite = TRUE)
  }
  
  return("Data files up to date :)")  

}

# ----------------load_rdata----------------
# loads an rdata object from the data dir into the environment
# searches in app/data/"regionStr" based on the input regionStr.
# Will raise a warning if the file is not found, but otherwise will not produce 
# an output.
# inputs: 
# rdataNames: list of string names of data to load
# regionStr: string defining the directory to search in, 
#            typically set and used from the config file.
load_rdata <- function(rdataNames, regionStr, env=globalenv()){
  data_dir_check()
  regionDir <- here::here("app/data", regionStr)
  lapply(rdataNames, find_and_load, regionDir = regionDir, env = env)
  return(NULL)
}

# ------------find_and_load-------------------
# Finds the appropriate rdata file in the give region in the data dir
find_and_load <- function(rdataStr, regionDir, env=globalenv()){
  fileName <- paste(rdataStr, ".RData", sep="")
  fileList <- list.files(regionDir, fileName, recursive=TRUE, full.names=TRUE, include.dirs=FALSE)
  if (length(fileList) == 1) {
    base::load(fileList, envir = env)
  } else if(length(fileList) == 0) {
    errMessage <- paste("R data file", fileName, "not found in", regionDir, ".")
    warning(errMessage)
  } else {
    errMessage <- paste("Duplicates of R data file", fileName, "found in", regionDir, ".")
    warning(errMessage)
  }
}

# ------------intro_setup----------------
# Loads common data needed for the report into the passed environment
# and initializes the base maps and summary tables for the report.
intro_setup <- function(studyArea, env=globalenv()) {
  source(here::here("config.R"), local=TRUE)
  copy_rdata_files() # make sure data files are up to date.
  #load common data into this function and passed environment
  load_rdata(c("CommonData"), regionStr, env = env)
  load_rdata(c("CommonData"), regionStr, env = environment())

  minYear <- rrMinYear
  site <- sf::st_centroid(studyArea)
  mapDataList <- maps_setup(studyArea, region_sf, land50k_sf, land10m_sf, bounds_sf)
  
  list2env(mapDataList, envir = env)
  
  summarySarTable <- data.frame("Species" = listed_species$`Common Name`, "COSEWIC" = listed_species$`COSEWIC status`, "SARA" = listed_species$`SARA status`)
  summaryHabTable <- data.frame("Species" = listed_species$`Common Name`, "COSEWIC" = listed_species$`COSEWIC status`, "SARA" = listed_species$`SARA status`, "Obs" = NA, "range" = NA, "SDM" = NA, "IH" = NA, "CH" = NA)
  summaryIntroTable <- data.frame(matrix(ncol = 2, nrow = 0))
  names(summaryIntroTable) <- c("Datasource", "Results")
  
  otherDataList <- list("site" = site,
                        "minYear" = minYear,
                        "summarySarTable" = summarySarTable,
                        "summaryHabTable" = summaryHabTable,
                        "summaryIntroTable" = summaryIntroTable,
                        "mapDataList" = mapDataList)
  
  list2env(otherDataList, envir = env)
  
  return("Data Setup Complete")
}
