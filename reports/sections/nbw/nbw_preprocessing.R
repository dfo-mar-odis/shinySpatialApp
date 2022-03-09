source(here::here("reports/dataprocessing/openDataHelpers.R"))
source(here::here("reports/R/dataFunctions.R"))

source(here::here("config.R"))



loadResult <- load_rdata(c("CommonData", "nbw_rr"), regionStr)

# -----------NBW-------------- 
nbwPkgId <- "9fd7d004-970c-11eb-a2f3-1860247f53e3"
nbwResId <- "f69a7d34-7c18-485b-98d7-8d45b7f8a3ce"

bwRefList <- list("en" = "<http://publications.gc.ca/collections/collection_2020/mpo-dfo/fs70-6/Fs70-6-2020-008-eng.pdf>",
                  "fr" = "<http://publications.gc.ca/collections/collection_2020/mpo-dfo/fs70-6/Fs70-6-2020-008-fra.pdf>")

save_open_data(nbwPkgId, nbwResId, "nbw_rr", highQuality, localFileSavePath,
               contactEmail = email_format("MaritimesRAP.XMAR\\@dfo-mpo.gc.ca"),  
               region_sf = region_sf, gdbLayer = "NorthernBottlenoseWhale_InterCanyonHabitat",
               reference = bwRefList)
