
copy_rdata_files <- function() {
  
  rDataDir <- "\\\\ent.dfo-mpo.ca\\ATLShares\\Science\\BIODataSvc\\IN\\MSP\\Data\\Rdata\\data"
  rStrDir <- gsub("\\\\", "\\\\\\\\", rDataDir) # backslash escaping nonsense
  
  localDataDir <- here::here("app/data")
  localStrDir <- gsub("\\\\", "\\\\\\\\", localDataDir) # backslash escaping nonsense
  
  
  remoteInfo <- file.info(list.files(rDataDir, recursive = TRUE, full.names = TRUE))
  remoteInfo["filenames"] <- sub(rStrDir, "", rownames(remoteInfo))
  
  localInfo <- file.info(list.files(localDataDir, recursive = TRUE, full.names = TRUE))
  localInfo["filenames"] <- sub(localStrDir, "", rownames(localInfo))
  
  missingList <- !(remoteInfo$filenames %in% localInfo$filenames) 
  if (any(missingList)) {
    missingFileList <- filter(remoteInfo, missingList)$filenames
    missingFilePath <- rownames(filter(remoteInfo, remoteInfo$filename %in% missingFileList))
    destList <- file.path(localDataDir, missingFileList)
    copyResults <- file.copy(missingFilePath, destList, overwrite = TRUE)
  }
  
  allInfo <- merge(remoteInfo, localInfo, by="filenames")
  updateList <- allInfo$mtime.x > allInfo$mtime.y
  if (any(updateList)) {
    updateFileList <- filter(allInfo, updateList)$filenames
    updateFilePath <- rownames(filter(remoteInfo, remoteInfo$filename %in% updateFileList))
    destList <- file.path(localDataDir, updateFileList)
    copyResults <- file.copy(updateFilePath, destList, overwrite = TRUE)
  }
  
  return("Data files up to date :)")  

}


load_rdata <- function(rdataNames, regionStr, env=globalenv()){
  regionDir <- here::here("app/data", regionStr)
  lapply(rdataNames, find_and_load, regionDir = regionDir, env = env)
  return(NULL)
}


find_and_load <- function(rdataStr, regionDir, env=globalenv()){
  fileName <- paste(rdataStr, ".RData", sep="")
  fileList <- list.files(regionDir, fileName, recursive=TRUE, full.names=TRUE, include.dirs=FALSE)
  if (length(fileList) == 1) {
    load(fileList, envir = env)
  } else if(length(fileList) == 0) {
    errMessage <- paste("R data file", fileName, "not found in", regionDir, ".")
    warning(errMessage)
  } else {
    errMessage <- paste("Duplicates of R data file", fileName, "found in", regionDir, ".")
    warning(errMessage)
  }
}

