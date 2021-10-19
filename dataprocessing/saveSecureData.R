
source(here::here("dataprocessing/openDataHelpers.R"))
source(here::here("app/R/dataFunctions.R"))


region_sf <- st_read(here::here("app/studyAreaTest/geoms_slc_MarBioRegion.geojson"))
setwd(here::here("app/data/MAR"))
setwd("\\\\ent.dfo-mpo.ca\\ATLShares\\Science\\BIODataSvc\\IN\\MSP\\Data")

loadResult <- load_rdata(c("conservationSites_rr"),  "MAR")


highQuality <- list("en" = "High", "fr" = "Élevée")
noneList <- list("en" = "None", "fr"= "Aucun")
internalUse <- list("en" = "DFO INTERNAL USE ONLY", "fr" = "DFO INTERNAL USE ONLY")

# ----------------EBSA----------------- 
EBSApkgId <- "d2d6057f-d7c4-45d9-9fd9-0a58370577e0"
EBSAresId <- "ec990fd7-91b0-4dbb-a0f4-bb11070a84c1"

EBSAcheckDate <- get_check_date("EBSA_rr")

OpenEBSA_rr <- get_opendata_rr(EBSApkgId, EBSAresId, region_sf = region_sf,
                               checkDate = EBSAcheckDate)
if(!is.null(OpenEBSA_rr)) {
  EBSA_rr <- OpenEBSA_rr
  EBSA_rr$data_sf$Report_URL <- str_replace(EBSA_rr$data_sf$Report_URL,
                                            ".pdf", ".html")
  EBSA_rr$qualityTier <- list("en" = "High", "fr" = "Élevée")
  EBSA_rr$contact <- list("en" = "[carissa.philippe\\@dfo-mpo.gc.ca](mailto:carissa.philippe@dfo-mpo.gc.ca){.email}",
                          "fr" = "[carissa.philippe\\@dfo-mpo.gc.ca](mailto:carissa.philippe@dfo-mpo.gc.ca){.email}")
  save(EBSA_rr, file = "./Open/EBSA_rr.RData")
}



# Marine Protected Areas (mpa)
conservationSites_raw <- st_read("../Data/Management/MPAN-Draft/MPAN_DraftDesign_Maritimes/MPAN_DraftDesign_Maritimes.shp", stringsAsFactors = FALSE)
conservationSites_sf <- st_transform(conservationSites_raw, crs = 4326)
conservationSites_sf <- st_make_valid(conservationSites_sf)
conservationSites_sf$Legend <- as.factor(conservationSites_sf$STATUS)
levels(conservationSites_sf$Legend) <- list("Existing Network Site"="Existing", "Other Network Sites"="Proposed", "Area of Interest for Oceans Act MPA"="Proposed AOI", "Proposed Conservation Area Under the Fisheries Act"="Proposed SBA")
conservationSites_sf$Legend <- as.character(conservationSites_sf$Legend)
conservationSites_sf[conservationSites_sf$Id == 18, "Legend" ] <- "Bras d'Or Lake -sites to be determined"

conservationSites_rr <- list("title" = "Draft Conservation Network Design",
                             "contact" = "Marty King ([Marty.King@dfo-mpo.gc.ca](mailto:Marty.King@dfo-mpo.gc.ca){.email})", 
                             "accessedOnStr" = list("en" ="October 2021", "fr" = "octobre, 2021") ,
                             "accessDate" = as.Date("2021-10-01"),
                             "data_sf" = conservationSites_sf,
                             "attribute" = "Legend",
                             "securityLevel" = noneList,
                             "qualityTier" = highQuality,
                             "constraints" = internalUse
)
save(conservationSites_rr, file = "./Secure/conservationSites_rr.RData")

