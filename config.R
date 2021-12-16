regionStr <- "MAR"

# Dir where the remote RData files are stored
remoteFileSavePath <- file.path("//ent.dfo-mpo.ca","ATLShares", "Science", "BIODataSvc", 
                          "IN", "MSP", "Data", "RData", "data", regionStr)

# Dir where the local RData files are stored
localFileSavePath <- here::here("app/data", regionStr)

# Dir where the raw data files are stored
fileLoadPath <- file.path("//ent.dfo-mpo.ca","ATLShares", "Science", "BIODataSvc", 
                          "IN", "MSP", "Data")

rr_openDataList <- data.frame("rrStr" = c("ebsa_rr", "crithab_rr", "sardist_rr", 
                                          "nbw_rr", "blueWhaleHab_rr", "finWhale_rr", 
                                          "rv_rr", "pasBay_rr", "benthicEffort_rr"))