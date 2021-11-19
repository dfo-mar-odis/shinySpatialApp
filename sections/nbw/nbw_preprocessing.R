source(here::here("dataprocessing/openDataHelpers.R"))
source(here::here("app/R/dataFunctions.R"))

fileSavePath <- "\\\\ent.dfo-mpo.ca\\ATLShares\\Science\\BIODataSvc\\IN\\MSP\\Data\\RData\\data\\MAR"
fileSavePath <- here::here("app/data/MAR")
fileLoadPath <- "\\\\ent.dfo-mpo.ca\\ATLShares\\Science\\BIODataSvc\\IN\\MSP\\Data"

loadResult <- load_rdata(c("CommonData", "nbw_rr"), "MAR")

# -----------NBW-------------- 
nbwPkgId <- "9fd7d004-970c-11eb-a2f3-1860247f53e3"
nbwResId <- "f69a7d34-7c18-485b-98d7-8d45b7f8a3ce"

bwRefList <- list("en" = "<http://publications.gc.ca/collections/collection_2020/mpo-dfo/fs70-6/Fs70-6-2020-008-eng.pdf>",
                  "fr" = "<http://publications.gc.ca/collections/collection_2020/mpo-dfo/fs70-6/Fs70-6-2020-008-fra.pdf>")

save_open_data(nbwPkgId, nbwResId, "nbw_rr", highQuality, fileSavePath,
               contactEmail = email_format("MaritimesRAP.XMAR\\@dfo-mpo.gc.ca"),  
               region_sf = region_sf, gdbLayer = "NorthernBottlenoseWhale_InterCanyonHabitat",
               reference = bwRefList)
