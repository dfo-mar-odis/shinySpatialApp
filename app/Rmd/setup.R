
###### Modify file formats ######
fin_whale_sf<-st_as_stars(fin_whale)%>%st_as_sf()

harbour_porpoise_sf<-st_as_stars(harbour_porpoise)%>%st_as_sf()

humpback_whale_sf<-st_as_stars(humpback_whale)%>%st_as_sf()

sei_whale_sf<-st_as_stars(sei_whale)%>%st_as_sf()

####### Filter files used in multiple sections by minYear  #######

obis_sf <- obis_sf %>% dplyr::filter(year >= minYear)
wsdb <- wsdb %>% dplyr::filter(YEAR >= minYear)
whitehead$YEAR<-lubridate::year(whitehead$Date)
whitehead <- whitehead %>% dplyr::filter(YEAR >= minYear)
narwc <- narwc %>% dplyr::filter(YEAR >= minYear)

####### Define colour coding for all cetacean plots for consistency
whale_col=values=c("Blue Whale: Endangered (SARA & COSEWIC)"="darkgoldenrod1",
                   "Fin Whale: Special Concern (SARA & COSEWIC)"="chartreuse4",
                   "Harbour Porpoise: Threatened (SARA) Special Concern (COSEWIC)"="black",
                   "Killer Whale: No Status (SARA) & Special Concern (COSEWIC)"="#00AFBB",
                   "North Atlantic Right Whale: Endangered (SARA & COSEWIC)"="darkorchid4",
                   "Northern Bottlenose Whale: Endangered (SARA & COSEWIC)"="#0827EF",
                   "Sei Whale: No Status (SARA) & Endangered (COSEWIC)"="#EF6408",
                   "Sowerby's Beaked Whale: Special Concern (SARA & COSEWIC)"="#F5A4E7"
                   )




