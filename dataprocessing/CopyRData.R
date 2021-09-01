
copy_rdata_files <- function() {
  rDataDir <- "\\\\ent.dfo-mpo.ca\\ATLShares\\Science\\BIODataSvc\\IN\\MSP\\Data\\Rdata\\data"
  dataFileList <- list.files(rDataDir, full.names = TRUE)
  file.copy(dataFileList, here::here("app/data/"), overwrite = TRUE)
}
