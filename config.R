regionStr <- "MAR"
rrMinYear <- 2010
rrBlue <- "#56B4E9"
# Dir where the remote RData files are stored
remoteFileSavePath <- file.path("//ent.dfo-mpo.ca","ATLShares", "Science", "BIODataSvc", 
                          "IN", "MSP", "Data", "RData", "data", regionStr)

# Dir where the local RData files are stored
localFileSavePath <- here::here("app/data", regionStr)

# Dir where the raw data files are stored
fileLoadPath <- file.path("//ent.dfo-mpo.ca","ATLShares", "Science", "BIODataSvc", 
                          "IN", "MSP", "Data")

rr_openDataList <- data.frame("rrStr" = c("ebsa_rr",  
                                          "nbw_rr", "blueWhaleHab_rr", "finWhale_rr", 
                                          "rv_rr", "pasBay_rr", "benthicEffort_rr",
                                          "fbWhale_rr", "bnWhale_rr", "greySeal_rr",
                                          "harbourPorpoise_rr", "harbourSeal_rr", 
                                          "imSalmon_rr", "imSalmonRivers_rr",
                                          "imLobster_rr", "narWhale_rr", "snowCrab_rr",
                                          "ssClam_rr", "tuna_rr", "imAcoustic_rr",
                                          "opprrpe_rr"))


githubRepo <- "https://github.com/dfo-mar-odis/shinySpatialApp/tree/main/"

