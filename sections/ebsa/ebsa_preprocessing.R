source(here::here("dataprocessing/openDataHelpers.R"))
source(here::here("app/R/dataFunctions.R"))

source(here::here("config.R"))



loadResult <- load_rdata(c("CommonData", "ebsa_rr"), regionStr)

# ----------------EBSA----------------- 
ebsaPkgId <- "d2d6057f-d7c4-45d9-9fd9-0a58370577e0"
ebsaResId <- "944c3b24-b861-4ddc-a111-03236d685dcf"

ebsaCheckDate <- get_check_date("ebsa_rr")

openEbsa_rr <- get_opendata_rr(ebsaPkgId, ebsaResId, region_sf = region_sf,
                               checkDate = ebsaCheckDate)
if(!is.null(openEbsa_rr)) {
  ebsa_rr <- openEbsa_rr
  ebsa_rr$metadata$qualityTier <- highQuality
  ebsa_rr$metadata$contact <- email_format("carissa.philippe\\@dfo-mpo.gc.ca")
  save(ebsa_rr, file = file.path(localFileSavePath, "Open/ebsa_rr.RData"))
}

