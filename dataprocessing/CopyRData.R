
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
  return("Data files up to date")  

}
