# contains functions for downloading open data records
source(here::here("dataprocessing/openDataHelpers.R"))

#contains load_rdata function
source(here::here("app/R/dataFunctions.R"))

#contains config parameters and paths
source(here::here("config.R"))

library(glatos)


# load in rr objects, CommonData contains data such as land borders, etc.
loadResult <- load_rdata(c("CommonData", "otn_rr"), regionStr)

# ---------------------OTN DETECTION EXTRACTS-----------------------------------
# LOADING AND PROCESSING CODE HERE

# If we are grabbing and tossing afterward:
# Concat and load all data in the subfolder dataprocessing/temp/OTN_detections/

# 



# rr object structure:
template_rr <- list("title" = "Template Title","data_sf" = "an sf object",
                    "attribute" = "Column to plot, or NONE",
                    "metadata" = list("contact" = email_format("email@email.com"), 
                                      "accessedOnStr" = list("en" ="January 1, 2022", "fr" = "1 Janvier, 2022") ,
                                      "accessDate" = as.Date("2022-01-01"),
                                      "searchYears" = "2022-2022",
                                      "securityLevel" = noneList,
                                      "qualityTier" = highQuality,
                                      "constraints" = internalUse
                    )
                    
)
save(template_rr, file = file.path(localFileSavePath, "Secure/template_rr.RData"))
