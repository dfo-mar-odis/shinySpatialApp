
source(here::here("reports/dataprocessing/openDataHelpers.R"))
source(here::here("reports/R/dataFunctions.R"))

source(here::here("config.R"))



loadResult <- load_rdata(c("CommonData", "ef_rr", "ws_rr", "rivers_rr"), regionStr)

# ---------------------ELECTROFISHING-----------------------------------

# Electrofishing
efSpecList <- data.frame("colNames" = c("AlosaUnidentified", "AmericanEel", "AtlanticSalmon", 
                                        "BlacknoseDace", "BrookTrout", "BrownTrout", 
                                        "ChubSpp", "CommonShiner", "CreekChub",  
                                        "GoldenShiner", "LakeChub", "Lamprey", "Mummichog", 
                                        "OtherCyprinids", "OtherSpp", "RainbowTrout",
                                        "SeaLamprey", "SlimySculpin", "SmallmouthBass", 
                                        "SticklebackSpp", "ThreespinedStickleback", "WhiteSucker"),
                         "Common.Names" = c("Unidentified Alosa", "American Eel", "Atlantic Salmon", 
                                            "Blacknose Dace", "Brook Trout", "Brown Trout", 
                                            "Chub Spp", "Common Shiner", "Creek Chub",  
                                            "Golden Shiner", "Lake Chub", "Lamprey", "Mummichog", 
                                            "Other Cyprinids", "Other Spp", "Rainbow Trout",
                                            "Sea Lamprey", "Slimy Sculpin", "Smallmouth Bass", 
                                            "Stickleback Spp", "Three spined Stickleback", "White Sucker"), 
                         "Scientific.Names" = c("Alosa", "Anguilla rostrata", "Salmo salar", 
                                                "Rhinichthys atratulus", "Salvelinus fontinalis", 
                                                "Salmo trutta", "Cyprinidae Spp",
                                                "Luxilus cornutus", "Semotilus atromaculatus",  
                                                "Notemigonus crysoleucas", "Couesius plumbeus", 
                                                "Petromyzontiformes", "Fundulus heteroclitus", 
                                                "Other Cyprinids", "Other Spp", "Oncorhynchus mykiss",
                                                "Petromyzon marinus", "Cottus cognatus",
                                                "Micropterus dolomieu", "Gasterosteidae Spp",
                                                "Gasterosteus aculeatus", "Catostomus commersonii"))


efSurvey_to_sf <- function(surveyData, efSpecList, region_sf){
  extraCols <- c("Year_", "DataSource", "Document")
  surveyLong <- tidyr::pivot_longer(surveyData, cols = any_of(efSpecList$colNames), names_to = "colNames", values_to = "count")
  surveyLong <- dplyr::filter(surveyLong, count > 0)
  surveyLong <- dplyr::filter(surveyLong, Year_ > 2008)
  surveyLong <- dplyr::select(surveyLong, all_of(c(extraCols, "Shape", "colNames")))
  surveyLong <- left_join(surveyLong, efSpecList, by = "colNames")
  survey_sf <- sf::st_transform(sf::st_as_sf(surveyLong), crs=4326)
  survey_sf <- sf::st_crop(survey_sf, region_sf)
  survey_sf <- dplyr::select(survey_sf, !colNames)
  names(survey_sf ) <- c("Year", "DataSource", "Document", "Common Name", "Scientific Name", "geometry")
  sf::st_geometry(survey_sf) <- "geometry"
  return(survey_sf)
}

efGdbPath <- file.path(fileLoadPath, "NaturalResources/Species/Electrofishing/EFishing_DFOScience.gdb")
sf::st_layers(efGdbPath)

efSurvey2009_sf <- sf::st_read(efGdbPath, layer = "EFish_ECB_2006_2007_BowlbyGibson2009" )
efSurvey2013_sf <- sf::st_read(efGdbPath, layer = "EFish_SU_2000_2008_RPA_2013" )
efSurvey2018A_sf <- sf::st_read(efGdbPath, layer = "EFish_IBoF_2014_nonLGB_Rivers_2018" )
efSurvey2018B_sf <- sf::st_read(efGdbPath, layer = "EFish_IBoF_2013_Stewiacke_2018" )

ef_sf <- efSurvey_to_sf(efSurvey2009_sf, efSpecList, region_sf)
ef_sf <- rbind(ef_sf, efSurvey_to_sf(efSurvey2013_sf, efSpecList, region_sf))
ef_sf <- rbind(ef_sf, efSurvey_to_sf(efSurvey2018A_sf, efSpecList, region_sf))
ef_sf <- rbind(ef_sf, efSurvey_to_sf(efSurvey2018B_sf, efSpecList, region_sf))


ef_rr <- list("title" = "Electrofishing Data","data_sf" = ef_sf,
              "attribute" = "Year",
              "metadata" = list("contact" = email_format("Dustin.Raab@dfo-mpo.gc.ca"), 
                                "accessedOnStr" = list("en" ="October 18 2021 by Sean Butler", "fr" = "18 octobre 2021 par Sean Butler") ,
                                "accessDate" = as.Date("2021-10-18"),
                                "searchYears" = "2008-2014",
                                "securityLevel" = noneList,
                                "qualityTier" = highQuality,
                                "constraints" = internalUse,
                                "pipelinePath" = paste0(githubRepo, "reports/sections/electrofishing/ef_preprocessing.R")
              )
              
)
save(ef_rr, file = file.path(localFileSavePath, "Secure/ef_rr.RData"))


# watersheds:

wsShpPath <- file.path(fileLoadPath, "NaturalResources/Species/Electrofishing/watersheds/NBNS_Watersheds.shp")
sf::st_layers(wsShpPath)

ws_sf <- sf::st_read(wsShpPath, crs=4326)
ws_sf <- sf::st_make_valid(ws_sf)
ws_sf <- sf::st_crop(ws_sf, region_sf)
ws_sf <- dplyr::select(ws_sf, c("WS_NAME", "geometry"))

ws_rr <- list("title" = "Watershed Boundries",
              "data_sf" = ws_sf,
              "attribute" = "NONE",
              "metadata" = list("contact" = "https://novascotia.ca/opendata/contact-us/, http://www.snb.ca/geonb1/e/contact/contact-E.asp", 
                                "accessedOnStr" = list("en" ="November 25 2021 by Quentin Stoyel", "fr" = "25 novembre 2021 par Quentin Stoye") ,
                                "accessDate" = as.Date("2021-11-25"),
                                "securityLevel" = noneList,
                                "qualityTier" = highQuality,
                                "constraints" = noneList,
                                "pipelinePath" = paste0(githubRepo, "reports/sections/electrofishing/ef_preprocessing.R")
              )
)
save(ws_rr, file = file.path(localFileSavePath, "Open/rivers_rr.RData"))

# rivers:

riversShpPath <- file.path(fileLoadPath, "NaturalResources/Species/Electrofishing/rivers/NBNS_rivers.shp")
sf::st_layers(riversShpPath)

rivers_sf <- sf::st_read(riversShpPath, crs=4326)
rivers_sf <- sf::st_make_valid(rivers_sf)
rivers_sf <- sf::st_crop(rivers_sf, region_sf)


rivers_rr <- list("title" = "Rivers",
              "data_sf" = rivers_sf,
              "attribute" = "NONE",
              "metadata" = list("contact" = "https://novascotia.ca/opendata/contact-us/, http://www.snb.ca/geonb1/e/contact/contact-E.asp", 
                                "accessedOnStr" = list("en" ="November 25 2021 by Quentin Stoyel", "fr" = "25 novembre 2021 par Quentin Stoye") ,
                                "accessDate" = as.Date("2021-11-25"),
                                "securityLevel" = noneList,
                                "qualityTier" = highQuality,
                                "constraints" = noneList,
                                "pipelinePath" = paste0(githubRepo, "reports/sections/electrofishing/ef_preprocessing.R")
              )
)
save(rivers_rr, file = file.path(localFileSavePath, "Open/rivers_rr.RData"))

