library(lubridate)
library(dplyr)
library(sf)

theSubUnitShape <- "c:/Users/mcmahonm/OneDrive - DFO-MPO/Support/RR/geometry/NAFO_polyBlocks.shp"
theSubUnitField <- "UnitArea"

nafo_sf <- st_read(theSubUnitShape)
nafo_sf$Shape_Leng<- nafo_sf$ORIG_FID <- nafo_sf$Shape_Le_1 <- nafo_sf$Shape_Area <- nafo_sf$OBJECTID <- NULL
colnames(nafo_sf)[colnames(nafo_sf)=="UnitArea"] <- "NAFO"
nafo_sf$NAFO <- toupper(nafo_sf$NAFO)
saveRDS(nafo_sf, file = "c:/Users/mcmahonm/OneDrive - DFO-MPO/Support/RR/geometry/NAFO_sf.rds")

#only need some MARFIS data, so let's not crash our computer
#only need some of the MARFIS data, so let's just get what we need. 
makeMarfis<-function(agg.poly.shp = theSubUnitShape, agg.poly.field= theSubUnitField){
  
  load("C:/DFO-MPO/wrangledData/MARFISSCI.PRO_SPC_INFO.RData")
  load("C:/DFO-MPO/wrangledData/MARFISSCI.NAFO_UNIT_AREAS.RData")
  load("C:/DFO-MPO/wrangledData/MARFISSCI.SPECIES.RData")
  marf <- PRO_SPC_INFO
  marf <- merge(marf,SPECIES[,c("SPECIES_CODE", "SPECIES_NAME")], all.x=T) 
  marf <- merge(marf, NAFO_UNIT_AREAS[,c("AREA_ID", "NAFO_AREA")], all.x = T, by.x="NAFO_UNIT_AREA_ID", by.y="AREA_ID")
  #Exclude buyer code 9004(confirmed this corresponds only with BUYER_ID 1643 which is in PRO_SPC_INFO
  marf <- marf[which(marf$BUYER_ID != 1643),]
  marfFields <- c ("PRO_SPC_INFO_ID", "YEAR", "YEAR_LANDED","LATITUDE", "LONGITUDE", "NAFO_AREA",
                   "SPECIES_CODE", "SPECIES_NAME", "RND_WEIGHT_KGS")
  rm(list=c("PRO_SPC_INFO", "NAFO_UNIT_AREAS", "SPECIES"))

  marf[marf$NAFO_AREA %in% c("OUTU","XXXX"),"NAFO_AREA"] <- NA
    
  #report is spatial - no coordinates or NAFO areas, need to drop
  marf <- marf[!is.na(marf$NAFO_AREA) | (!is.na(marf$LATITUDE) & !is.na(marf$LONGITUDE)),]
  marfCoords <- unique(marf[!is.na(marf$LATITUDE) & !is.na(marf$LONGITUDE), c("PRO_SPC_INFO_ID", "LATITUDE", "LONGITUDE")])
  marfCoords<-  Mar.utils::identify_area(df = marfCoords, lat.field = "LATITUDE", lon.field = "LONGITUDE", agg.poly.shp = agg.poly.shp, agg.poly.field = agg.poly.field)
  marfCoords$UnitArea <- toupper(marfCoords$UnitArea)
  marf <- merge(marf, marfCoords[,c("PRO_SPC_INFO_ID", "UnitArea")], all.x = T)
  rm(marfCoords)
  #drop the records that are outside of the range of the shp or are invalid
  marf <- marf[!is.na(marf$UnitArea) & !(marf$UnitArea %in% c("<OUTSIDE KNOWN AREAS>","<IMPOSSIBLE COORD>")),]
  #if no coordinate can be calculated, uese the supplied one instead
  marf$NAFO <- marf$NAFO_AREA
  marf[!is.na(marf$UnitArea),"NAFO"]<-marf[!is.na(marf$UnitArea),"UnitArea"]
  marf$UnitArea  <- marf$NAFO_AREA <- marf$LATITUDE <- marf$LONGITUDE <-  marf$PRO_SPC_INFO_ID <- NULL
  marf <- marf %>%
    group_by(YEAR_LANDED, SPECIES_CODE ,SPECIES_NAME ,NAFO) %>%
    summarise(RND_WEIGHT_KGS = sum(RND_WEIGHT_KGS), .groups = "keep")%>%
    as.data.frame()
  return(marf)
}

makeISDB <- function(agg.poly.shp = theSubUnitShape, agg.poly.field= theSubUnitField){
  load("C:/DFO-MPO/wrangledData/ISDB.ISCATCHES.RData")
  load("C:/DFO-MPO/wrangledData/ISDB.ISFISHSETS.RData")
  load("C:/DFO-MPO/wrangledData/ISDB.ISSPECIESCODES.RData")
  load("C:/DFO-MPO/wrangledData/ISDB.ISSETPROFILE_WIDE.RData")
  load("C:/DFO-MPO/wrangledData/ISDB.ISTRIPS.RData")
  
  isdb <- ISFISHSETS[,c("FISHSET_ID", "TRIP_ID", "NAFAREA_ID", "SETCD_ID")]
  isdb <- merge(isdb, ISCATCHES[,c("FISHSET_ID", "EST_COMBINED_WT", "SPECCD_ID")], all.x = T)
  isdb <- merge(isdb, ISSETPROFILE_WIDE[,c("YEAR","LATITUDE","LONGITUDE", "FISHSET_ID")], all.x = T)
  isdb <- merge(isdb, ISSPECIESCODES[, c("SPECCD_ID", "COMMON", "SCIENTIFIC")], all.x = T)
  isdb <- merge(isdb, ISTRIPS[,c("TRIP_ID", "TRIPCD_ID")], all.x = T)
  
  rm(list=c("ISFISHSETS", "ISCATCHES", "ISSETPROFILE_WIDE", "ISSPECIESCODES", "ISTRIPS"))
  #retain only commercial trip types (i.e. exclude industry surveys)
  isdb <- isdb[isdb$TRIPCD_ID <7010 | isdb$TRIPCD_ID == 7057| isdb$TRIPCD_ID == 7099,]
  #Commercial sets: retaining:
  #   1 - COMMERCIAL
  #   7 - COMMERCIAL - C AND P
  #  10 - COMMERCIAL INDEX
  isdb <- isdb[isdb$SETCD_ID %in% c(1,7,10),]
  isdb$TRIP_ID <- isdb$SETCD_ID <- NULL
  isdb <- isdb[!is.na(isdb$SPECCD_ID),]
  isdbCoords <- unique(isdb[!is.na(isdb$LATITUDE) & !is.na(isdb$LONGITUDE), c("FISHSET_ID", "LATITUDE", "LONGITUDE")])
  isdbCoords<-  Mar.utils::identify_area(df = isdbCoords, lat.field = "LATITUDE", lon.field = "LONGITUDE", agg.poly.shp = theSubUnitShape, agg.poly.field = theSubUnitField)
  isdbCoords$UnitArea <- toupper(isdbCoords$UnitArea)
  isdb <- merge(isdb, isdbCoords[,c("FISHSET_ID", "UnitArea")], all.x = T)
  rm(isdbCoords)
  #drop the records that are outside of the range of the shp or are invalid
  isdb <- isdb[!is.na(isdb$UnitArea) & !(isdb$UnitArea %in% c("<OUTSIDE KNOWN AREAS>","<IMPOSSIBLE COORD>")),]
  
  #if no coordinate can be calculated, uese the supplied one instead
  isdb$NAFO <- isdb$NAFAREA_ID
  isdb[!is.na(isdb$UnitArea),"NAFO"]<-isdb[!is.na(isdb$UnitArea),"UnitArea"]
  isdb$UnitArea  <- isdb$NAFAREA_ID <- isdb$LATITUDE <- isdb$LONGITUDE <-  isdb$FISHSET_ID <- NULL
  isdb <- isdb %>%
    group_by(YEAR, SPECCD_ID,COMMON,SCIENTIFIC,NAFO) %>%
    summarise(EST_COMBINED_WT = sum(EST_COMBINED_WT), .groups = "keep")%>%
    as.data.frame()
  return(isdb)
}

makeAnnualSfs<-function(data = NULL, yearField = NULL, savePath = getwd(), filename=NULL, nafoSf = nafo_sf){
  #remove data with no year
  data <- data[!is.na(data[,yearField]),]
  #remove current year and first year since neither will not have all of the data
  data <- data[data[,yearField] < lubridate::year(Sys.Date()),]
  data <- data[data[,yearField] > min(data[,yearField]),]
  allYrs <- sort(unique(data[,yearField]))
  for (y in 1:length(allYrs)){
    thisYr <- data[data[,yearField] == allYrs[y],]
    thisYr_sf <- merge(nafoSf, thisYr, all=T)
    saveRDS(thisYr_sf, file = file.path(savePath,paste0(filename,"_",allYrs[y],"_sf.rds")))
    rm(thisYr_sf)
  }
}

# Code below creates the files I shared, and requires data to already be on your computer 
#
if(F){
  marf <- makeMarfis()
  isdb <- makeISDB()
  saveRDS(marf, file = "c:/Users/mcmahonm/OneDrive - DFO-MPO/Support/RR/marfis/marfis_raw.rds")
  saveRDS(isdb, file = "c:/Users/mcmahonm/OneDrive - DFO-MPO/Support/RR/isdb/isdb_raw.rds")
  makeAnnualSfs(data = isdb, yearField = "YEAR", savePath = "c:/Users/McMahonM/OneDrive - DFO-MPO/Support/RR/isdb/", filename = "isdb")
  makeAnnualSfs(data = marf, yearField = "YEAR_LANDED", savePath = "c:/Users/McMahonM/OneDrive - DFO-MPO/Support/RR/marfis/", filename = "marfis")
}
#
#


# Code below makes example plots using the provided data
#

if (F){
  
marfis_2018_sf <- readRDS("C:/Users/McMahonM/OneDrive - DFO-MPO/Support/RR/marfis/marfis_2018_sf.rds")
isdb_2018_sf <- readRDS("C:/Users/McMahonM/OneDrive - DFO-MPO/Support/RR/isdb/isdb_2018_sf.rds")
m_halibut_2018 <- marfis_2018_sf %>% 
  filter(SPECIES_CODE ==130)
m_swordfish_2018 <- marfis_2018_sf %>% 
  filter(SPECIES_CODE ==251)  
i_seaurchins_2018 <- isdb_2018_sf %>% 
  filter(SPECCD_ID ==6400)
i_blueSharks_2018 <- isdb_2018_sf %>% 
  filter(SPECCD_ID ==231)

#get bbox of data
ext <- st_bbox(marfis_2018_sf)

library(ggplot2)
library(viridis)

#marfis plots
ggplot() + 
  ggtitle("MARFIS 2018: Halibut") +
  geom_sf(data = m_halibut_2018, aes(fill = RND_WEIGHT_KGS))+
  scale_fill_viridis_c() +
  geom_sf(data = Mar.data::coast_lores_sf) +
  coord_sf(xlim = c(ext[["xmin"]], ext[["xmax"]]), ylim = c(ext[["ymin"]], ext[["ymax"]]), expand = FALSE) 

ggplot() + 
  ggtitle("MARFIS 2018: Swordfish") +
  geom_sf(data = m_swordfish_2018, aes(fill = RND_WEIGHT_KGS))+
  scale_fill_viridis_c() +
  geom_sf(data = Mar.data::coast_lores_sf) +
  coord_sf(xlim = c(ext[["xmin"]], ext[["xmax"]]), ylim = c(ext[["ymin"]], ext[["ymax"]]), expand = FALSE) 
#isdb plots
ggplot() + 
  ggtitle("ISDB 2018: Sea Urchins") +
  geom_sf(data = i_seaurchins_2018, aes(fill = EST_COMBINED_WT))+
  scale_fill_viridis_c() +
  geom_sf(data = Mar.data::coast_lores_sf) +
  coord_sf(xlim = c(ext[["xmin"]], ext[["xmax"]]), ylim = c(ext[["ymin"]], ext[["ymax"]]), expand = FALSE) 

ggplot() + 
  ggtitle("ISDB 2018: Blue Sharks") +
  geom_sf(data = i_blueSharks_2018, aes(fill = EST_COMBINED_WT))+
  scale_fill_viridis_c() +
  geom_sf(data = Mar.data::coast_lores_sf) +
  coord_sf(xlim = c(ext[["xmin"]], ext[["xmax"]]), ylim = c(ext[["ymin"]], ext[["ymax"]]), expand = FALSE) 


}
#



