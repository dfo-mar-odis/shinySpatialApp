source(here::here("dataprocessing/openDataHelpers.R"))
source(here::here("app/R/dataFunctions.R"))

fileSavePath <- "\\\\ent.dfo-mpo.ca\\ATLShares\\Science\\BIODataSvc\\IN\\MSP\\Data\\RData\\data\\MAR"
fileSavePath <- here::here("app/data/MAR")
fileLoadPath <- "\\\\ent.dfo-mpo.ca\\ATLShares\\Science\\BIODataSvc\\IN\\MSP\\Data"

loadResult <- load_rdata(c("CommonData", "ebsa_rr"), "MAR")


# -----------SDM--------------
sdmPkgId <- "c094782e-0d6f-4cc0-b5a3-58908493a433"
sdmResId <- "16df15fb-367c-46e3-8ab7-be25315b9fbd"

save_open_data(sdmPkgId, sdmResId, "finWhale_rr", mediumQuality, fileSavePath,
               contactEmail = email_format("Hilary.Moors-Murphy\\@dfo-mpo.gc.ca"),  
               region_sf = region_sf, tifFile = "Fin_Whale.tif", searchYears="1975-2015",
               reference = lang_list("<https://waves-vagues.dfo-mpo.gc.ca/Library/40869155.pdf>"))

save_open_data(sdmPkgId, sdmResId, "seiWhale_rr", mediumQuality, fileSavePath,
               contactEmail = email_format("Hilary.Moors-Murphy\\@dfo-mpo.gc.ca"),  
               region_sf = region_sf, tifFile = "Sei_Whale.tif", searchYears="1975-2015",
               reference = lang_list("<https://waves-vagues.dfo-mpo.gc.ca/Library/40869155.pdf>"))

save_open_data(sdmPkgId, sdmResId, "humpbackWhale_rr", mediumQuality, fileSavePath,
               contactEmail = email_format("Hilary.Moors-Murphy\\@dfo-mpo.gc.ca"),  
               region_sf = region_sf, tifFile = "Humpback_Whale.tif", searchYears="1975-2015",
               reference = lang_list("<https://waves-vagues.dfo-mpo.gc.ca/Library/40869155.pdf>"))

save_open_data(sdmPkgId, sdmResId, "harbourPorpoise_rr", mediumQuality, fileSavePath,
               contactEmail = email_format("Hilary.Moors-Murphy\\@dfo-mpo.gc.ca"),  
               region_sf = region_sf, tifFile = "Harbour_Porpoise.tif", searchYears="1975-2015",
               reference = lang_list("<https://waves-vagues.dfo-mpo.gc.ca/Library/40869155.pdf>"))
