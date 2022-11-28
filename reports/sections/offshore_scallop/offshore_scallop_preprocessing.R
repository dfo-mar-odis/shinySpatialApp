# contains functions for downloading open data records
source(here::here("reports/dataprocessing/openDataHelpers.R"))

#contains load_rdata function
source(here::here("reports/R/dataFunctions.R"))

#contains config parameters and paths
source(here::here("config.R"))

library(lubridate) # load library to use year/month/day as functions and headers

# load in rr objects, CommonData contains data such as land borders, etc.
loadResult <- load_rdata(c("CommonData", "offshoreScallop_rr"), regionStr)

# ---------------------Offshore Scallop-----------------------------------

load("R:/Science/BIODataSvc/IN/MSP/Data/NaturalResources/Species/OffshoreScallops/OSdat.RData")

#format date and separate y m d
dat$TOW_DATE <- as.factor(as.Date(dat$TOW_DATE, format = "%Y-%m-%d"))
dat1 <- dat %>% 
  dplyr::mutate(TOW_DATE = lubridate::ymd(TOW_DATE)) %>% 
  dplyr::mutate_at(vars(TOW_DATE), dplyr::funs(year, month, day))

#remove GBUSA data, LURCHER and SPB data
dat_sf <- dat1 %>%
  dplyr::filter(!bank %in% c("GBUSA", "LURCHER", "SPB")) %>%
  dplyr::filter(year!="2020") %>% 
  dplyr::select(year, month, bank, TOW_NO, sLon, sLat) %>% #select required columns
  distinct() %>%
  sf::st_as_sf(coords=c("sLon", "sLat"), crs=4326) %>% #set coordinate system
  sf::st_intersection(region_sf) #trim to region

banknames <- data.frame(bank = c("BBn", "BBs", "Ger", "Ban", "Mid", "Sab", 
                                 "GBa", "GBb"), 
                        name = c("Browns North", "Browns South", "German", 
                                 "Banquereau", "Middle", "Sable", "Georges 'a'", 
                                 "Georges 'b'"))

banknames$name <- factor(banknames$name, 
                         levels = c("Browns North", "Browns South", "German", 
                                    "Banquereau", "Middle", "Sable", 
                                    "Georges Bank Monitoring", "Georges 'a'",
                                    "Georges 'b'"))

#create labels for the plot
bankLabels <- dat_sf %>% 
  dplyr::group_by(bank) %>%
  summarize() %>% 
  sf::st_centroid() %>%
  cbind(st_coordinates(.)) %>% 
  dplyr::left_join(banknames)


# rr object structure:
offshoreScallop_rr <- list(
  "title" = "Offshore Scallop Survey",
  "data_sf" = dat_sf,
  "attribute" = "NONE",
  "metadata" = list("contact" = email_format("freya.keyser@dfo-mpo.gc.ca"), 
                    "accessedOnStr" = list("en" ="May 4, 2022", 
                                           "fr" = "4 Mai, 2022") ,
                    "accessDate" = as.Date("2022-05-04"),
                    "searchYears" = "1978-2021",
                    "securityLevel" = protectedBList,
                    "qualityTier" = highQuality,
                    "constraints" = internalUse,
                    "pipelinePath" = paste0(githubRepo, "reports/sections/offshore_scallop/offshore_scallop_preprocessing.R")
                    
                    )
  )
save(offshoreScallop_rr, bankLabels, file = file.path(remoteFileSavePath, "Protected/offshoreScallop_rr.RData"))

