regionStr <- "MAR"

# Dir where the remote RData files are stored
remoteFileSavePath <- file.path("//ent.dfo-mpo.ca","ATLShares", "Science", "BIODataSvc", 
                          "IN", "MSP", "Data", "RData", "data", regionStr)

# Dir where the local RData files are stored
localFileSavePath <- here::here("app/data", regionStr)

# Dir where the raw data files are stored
fileLoadPath <- file.path("//ent.dfo-mpo.ca","ATLShares", "Science", "BIODataSvc", 
                          "IN", "MSP", "Data")