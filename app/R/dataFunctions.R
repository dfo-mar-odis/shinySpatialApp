
copy_rdata_files <- function() {
  
  rDataDir <- "\\\\ent.dfo-mpo.ca\\ATLShares\\Science\\BIODataSvc\\IN\\MSP\\Data\\Rdata\\data"
  
  localDataDir <- here::here("app/data/")
  remoteInfo <- file.info(list.files(rDataDir, full.names = TRUE))
  remoteInfo["filenames"] = list.files(rDataDir)
  localInfo <- file.info(list.files(localDataDir, full.names = TRUE))
  localInfo["filenames"] = list.files(localDataDir)
  
  allInfo <- merge(remoteInfo, localInfo, by="filenames")
  updateList <- allInfo$mtime.x > allInfo$mtime.y
  if (any(updateList)) {
    updateFileList <- filter(allInfo, updateList)$filenames
    updateFilePath <- rownames(filter(remoteInfo, remoteInfo$filename %in% updateFileList))
    copyResults <- file.copy(updateFilePath, localDataDir, overwrite = TRUE)
  }
  return("Data files up to date :)")  

}


load_rdata <- function(rdataNames, regionStr, env=globalenv()){
  regionDir <- here::here("app/data", regionStr)
  lapply(rdataNames, find_and_load, regionDir = regionDir, env = env)
}


find_and_load <- function(rdataStr, regionDir, env=globalenv()){
  fileName <- paste(rdataStr, ".RData", sep="")
  fileList <- list.files(regionDir, fileName, recursive=TRUE, full.names=TRUE, include.dirs=FALSE)
  if (length(fileList) == 1) {
    load(fileList, envir = env)
    return(TRUE)
  } else if(length(fileList) == 0) {
    errMesage <- paste("R data file", fileName, "not found in", regionDir, ".")
    warning(errMessage)
    return(NULL)
  } else {
    errMesage <- paste("Duplicates of R data file", fileName, "found in", regionDir, ".")
    warning(errMessage)
    return(NULL)
  }
}

