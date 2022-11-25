# contains functions for downloading open data records
source(here::here("dataprocessing/openDataHelpers.R"))

#contains load_rdata function
source(here::here("app/R/dataFunctions.R"))

#contains config parameters and paths
source(here::here("config.R"))


# load in rr objects, CommonData contains data such as land borders, etc.
loadResult <- load_rdata(c("CommonData", "nitroLoad_rr"), regionStr)

# ----------------Nitro----------------- 
nitroPkgId <- "08746031-1970-4bf6-b6d4-3de2715c8634"
nitroResId <- "cb3a3d96-bf2c-4403-a5c3-bed4ff54d1a5"

nitroCheckDate <- get_check_date("nitroLoad_rr")

totalNitroLoad_rr <- get_opendata_rr(nitroPkgId, nitroResId, region_sf = region_sf,
                               checkDate = nitroCheckDate, gdbLayer = "Watershed_AnthropogenicNitrogenLoading_TotalN_Load")
deltaNitroLoad_rr <- get_opendata_rr(nitroPkgId, nitroResId, region_sf = region_sf,
                               checkDate = nitroCheckDate, gdbLayer = "Embayment_AnthropogenicNitrogenLoading_DeltaN")

totalNitroLoad_rr$attribute <- "Total_Nitrogen_Load"
totalNitroLoad_rr$metadata$contact <- email_format("Noreen.Kelly\\@dfo-mpo.gc.ca")
totalNitroLoad_rr$metadata$qualityTier <- mediumQuality
totalNitroLoad_rr$metadata$pipelinePath <- paste0(githubRepo, "reports/sections/nitroLoad/nitroLoad_preprocessing.R")
totalNitroLoad_rr$metadata$searchYears <- "2000-2020?"


deltaNitroLoad_rr$attribute <- "Mean_DeltaN"
deltaNitroLoad_rr$metadata$contact <- email_format("Noreen.Kelly\\@dfo-mpo.gc.ca")
deltaNitroLoad_rr$metadata$qualityTier <- mediumQuality
totalNitroLoad_rr$metadata$pipelinePath <- paste0(githubRepo, "reports/sections/nitroLoad/nitroLoad_preprocessing.R")
totalNitroLoad_rr$metadata$searchYears <- "2000-2020?"

save(totalNitroLoad_rr, deltaNitroLoad_rr, file = file.path(localFileSavePath, "Open/nitroLoad_rr.RData"))


