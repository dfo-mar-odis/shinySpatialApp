source(here::here("config.R"))
source(here::here("reports/dataprocessing/openDataHelpers.R"))
source(here::here("reports/R/dataFunctions.R"))

loadResult <- load_rdata(c("CommonData"), regionStr)

print("----------------COMMON DATA-------------")
if (globalControlEnv$updateGeoms) {
  
  region_sf <- sf::st_read(file.path(fileLoadPath, "Boundaries/MaritimesRegionBound/geoms_slc_MarBioRegion.geojson"))
  if (regionStr == "QC") {
    region_sf <- sf::st_read(file.path(fileLoadPath, "Boundaries/MaritimesRegionBound/geoms_slc_MarBioRegion.geojson"))
  }
  land10m_sf <- sf::st_read(file.path(fileLoadPath, "Boundaries/Landmass/ne_10m_land_Clip.shp"), stringsAsFactors = FALSE)
  #remove State and Province column from land10m
  land10m_sf <- land10m_sf[-c(2)]
  
  land50k_sf <- sf::st_read(file.path(fileLoadPath, "Boundaries/Coast50k/Coastline50k_SHP/Land_AtlCanada_ESeaboardUS.shp"),
                        stringsAsFactors = FALSE)
  
  # National boundaries (terrestrial and marine)
  bounds_sf <- sf::st_read(file.path(fileLoadPath, "Boundaries/AdminBoundaries/AdminBounds_SHP/Boundaries_Line.shp"), stringsAsFactors = FALSE)
  bounds_sf <- dplyr::select(bounds_sf,SRC_DESC, geometry)
  bounds_sf <- sf::st_transform(bounds_sf, 4326) # Project to WGS84
  
  # Table of SARA listed species
  listed_species <- read.csv(file.path(fileLoadPath, "NaturalResources/Species/MAR_listed_species.csv"),
                             stringsAsFactors = FALSE)
  listed_species <- listed_species %>% dplyr::rename("SARA status" = Schedule.status,
                                              "COSEWIC status" = COSEWIC.status,
                                              "Wild Species listing" = Wild_Species,
                                              "SCIENTIFICNAME" = Scientific_Name_upper,
                                              "COMMONNAME" = Common_Name_upper,
                                              "Scientific Name" = Scientific_Name,
                                              "Common Name" = Common_Name)
  row.names(listed_species) <- NULL
  
  # Cetacean legend file
  cetLegend <- data.frame("Scientific_Name" = c("Delphinapterus leucas","Balaenoptera musculus", "Balaenoptera physalus",
                                                "Phocoena phocoena", "Orcinus orca", "Eubalaena glacialis",
                                                "Hyperoodon ampullatus", "Balaenoptera borealis", "Mesoplodon bidens"),
                          "Legend" = c("Beluga Whale: Endangered (SARA & COSEWIC)", 
                                       "Blue Whale: Endangered (SARA & COSEWIC)",
                                       "Fin Whale: Special Concern (SARA & COSEWIC)",
                                       "Harbour Porpoise: Threatened (SARA) Special Concern (COSEWIC)",
                                       "Killer Whale: No Status (SARA) & Special Concern (COSEWIC)",
                                       "North Atlantic Right Whale: Endangered (SARA & COSEWIC)",
                                       "Northern Bottlenose Whale: Endangered (SARA & COSEWIC)",
                                       "Sei Whale: No Status (SARA) & Endangered (COSEWIC)",
                                       "Sowerby's Beaked Whale: Special Concern (SARA & COSEWIC)")
  )
  
  cetLegend <- dplyr::rename(cetLegend, c("Scientific Name" = "Scientific_Name"))
  
  rr_otherSpecies <- data.frame("Common_Name" = c("LOGGERHEAD SEA TURTLE", "ATLANTIC WALRUS",
                                            "HARBOUR SEAL LACS DES LOUPS MARINS SUBSPECIES", 
                                            "LEATHERBACK SEA TURTLE"),
                          "Scientific_Name" = c("Odobenus rosmarus  rosmarus", 
                                                "Phoca vitulina mellonae", 
                                                "Dermochelys coriacea",
                                                "Caretta caretta"))
}
save(region_sf, land10m_sf, land50k_sf, bounds_sf, listed_species, cetLegend, rr_otherSpecies,
     file = file.path(get_file_save_path(globalControlEnv$saveToRemote), "CommonData.RData"))
