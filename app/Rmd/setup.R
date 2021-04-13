###### studyArea ######
# define studyBox geometry "look". 
# studyBox_geom is input into area map or can be added to any map later
land_layer<-land50k_sf
studyBox_geom <- geom_sf(data=studyArea, fill=NA, col="red", size=1) # Define studyBox geometry "look".
areaMapList<-area_map(studyArea,site,land_layer, 5, bounds_sf,studyBox_geom)
# separate items in the output list: first item is a map and second is a bounding box of the map
areaMap=areaMapList[[1]]
bboxMap=areaMapList[[2]]

###### Set up requirements for the report, Gregory Puncher, Winter 2021 ######

fin_whale_sf<-st_as_stars(fin_whale)%>%st_as_sf()

harbour_porpoise_sf<-st_as_stars(harbour_porpoise)%>%st_as_sf()

humpback_whale_sf<-st_as_stars(humpback_whale)%>%st_as_sf()

sei_whale_sf<-st_as_stars(sei_whale)%>%st_as_sf()

## Filter files used in multiple sections by minYear  

obis_sf <- obis_sf %>% dplyr::filter(year >= minYear)
wsdb <- wsdb %>% dplyr::filter(YEAR >= minYear)
whitehead$YEAR<-lubridate::year(whitehead$Date)
whitehead <- whitehead %>% dplyr::filter(YEAR >= minYear)
narwc <- narwc %>% dplyr::filter(YEAR >= minYear)

## Define colour coding for all cetacean plots for consistency
whale_col=values=c("Blue Whale: Endangered (SARA & COSEWIC)"="black",
                   "Fin Whale: Special Concern (SARA & COSEWIC)"="chartreuse4",
                   "Harbour Porpoise: Threatened (SARA) Special Concern (COSEWIC)"="darkgoldenrod1",
                   "Killer Whale: No Status (SARA) & Special Concern (COSEWIC)"="#00AFBB",
                   "North Atlantic Right Whale: Endangered (SARA & COSEWIC)"="darkorchid4",
                   "Northern Bottlenose Whale: Endangered (SARA & COSEWIC)"="#0827EF",
                   "Sei Whale: No Status (SARA) & Endangered (COSEWIC)"="#EF6408",
                   "Sowerby's Beaked Whale: Special Concern (SARA & COSEWIC)"="#F5A4E7"
                   )




