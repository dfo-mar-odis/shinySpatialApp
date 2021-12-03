regionStr <- "MAR"

remoteFileSavePath <- file.path("//ent.dfo-mpo.ca","ATLShares", "Science", "BIODataSvc", 
                          "IN", "MSP", "Data", "RData", "data", regionStr)

localFileSavePath <- here::here("app/data", regionStr)


fileLoadPath <- file.path("//ent.dfo-mpo.ca","ATLShares", "Science", "BIODataSvc", 
                          "IN", "MSP", "Data")
