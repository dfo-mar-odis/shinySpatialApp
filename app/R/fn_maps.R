# #site map
 
site_map <- function(studyArea,site,land_layer,buf, bound) {
  
  # buf is in km, and now converted to degrees
  buf=buf/100
  
  # bounding box
  bbox=st_bbox(studyArea)
  
  # longitude and latitude limits for the map
  longmin<-(bbox$xmin)-buf
  longmax<-bbox$xmax+buf
  latmin<-bbox$ymin-buf
  latmax<-bbox$ymax+buf
  
  ggplot()+
    geom_sf(data=studyArea,fill="deepskyblue", col="black", size=0.6, alpha=0.4)+
    geom_sf(data=site,fill="yellow",col="black", size=0.6)+
    geom_sf(data=bound, col = "red")+ # creates US boundary line, 200 nm limit
    geom_sf(data=land_layer,fill=c("lightgrey"), col="black", size=0.7)+
    watermark(show = TRUE, lab = "DFO Internal Use Only")+
    annotation_scale(location="bl")+
    theme_bw()+
    coord_sf(xlim = c(longmin, longmax), ylim = c(latmin, latmax))+
    labs(x="Longitude", y="Latitude", col="")+
    theme(axis.title.y = element_text(size = 13))+
    theme(axis.title.x = element_text(size = 13))
  
}

########## Species At Risk distribution and Critical Habitat data ##########

# critical habitat
plot_crithab<-function(ClippedCritHab_sf, studyArea, land_layer, buf, bound) {
  
  # buf is in km, and now converted to degrees
  buf=buf/100
  buf_lat=buf*0.72
  
  # bounding box
  bbox <- st_bbox(studyArea)
  
  # longitude and latitude limits for the map
  longmin<-(bbox$xmin)-buf
  longmax<-bbox$xmax+buf
  latmin<-bbox$ymin-buf_lat
  latmax<-bbox$ymax+buf_lat
  
  ggplot()+
    geom_sf(data=studyArea, fill="#74ECFB", col="black", size=0.6)+
    geom_sf(data=leatherback_sf,fill="lightgreen",col="black")+
    geom_sf(data=ClippedCritHab_sf,fill="red",col="black")+
    geom_sf(data=bound, col = "red")+ # creates US boundary line, 200 nm limit
    geom_sf(data=land_layer,fill=c("grey90"), col="black")+
    annotation_scale(location="br")+
    theme_bw()+
    coord_sf(xlim = c(longmin, longmax), ylim = c(latmin, latmax))+
    labs(x=expression(paste("Longitude ",degree,"W",sep="")),
         y=expression(paste("Latitude ",degree,"N",sep="")),
         col="")+
    watermark(show = TRUE, lab = "DFO Internal Use Only")
  
}

 
#SAR distribution
plot_sardist<-function(sardist_sf, studyArea, land_layer, buf, bound) {
  
  # buf is in km, and now converted to degrees
  buf=buf/100
  buf_lat=buf*0.72
  
  # bounding box
  bbox=st_bbox(studyArea)
  
  # longitude and latitude limits for the map
  longmin<-(bbox$xmin)-buf
  longmax<-bbox$xmax+buf
  latmin<-bbox$ymin-buf_lat
  latmax<-bbox$ymax+buf_lat
  
  ggplot()+
    geom_sf(data=sardist_sf,fill="orange", col="black", size=0.6)+    
    geom_sf(data=studyArea, fill="#74ECFB", col="black", size=0.6)+
    geom_sf(data=bound, col = "red")+ # creates US boundary line, 200 nm limit
    geom_sf(data=land_layer,fill=c("grey90"), col="black")+
    annotation_scale(location="br")+
    theme_bw()+
    coord_sf(xlim = c(longmin, longmax), ylim = c(latmin, latmax))+
    labs(x=expression(paste("Longitude ",degree,"W",sep="")),
         y=expression(paste("Latitude ",degree,"N",sep="")),
         col="")+
    watermark(show = TRUE, lab = "DFO Internal Use Only")
  
}
 
###### Cetacean section ##########

# #Grid of 4 cetacean priority habitat
 
plot_cetaceans_4grid<-function(fin_whale_sf, harbour_porpoise_sf,
                               humpback_whale_sf, sei_whale_sf, studyArea,
                               land_layer,buf, bound) {
  # buf is in km, and now converted to degrees
  buf=buf/100
  buf_long=buf*2
  
  # bounding box
  bbox=st_bbox(studyArea)
  
  # longitude and latitude limits for the map
  longmin<-(bbox$xmin)-buf_long
  longmax<-bbox$xmax+buf_long
  latmin<-bbox$ymin-buf
  latmax<-bbox$ymax+buf
  
  #Fin Whale
  
  fin_whale_plot <- ggplot()+
    geom_sf(data=studyArea, fill="#74ECFB", col="black", size=0.6)+
    geom_sf(data=fin_whale_sf,fill="#F3E73B",col="#F3E73B")+
    geom_sf(data=bound, col = "red")+ # creates US boundary line, 200 nm limit
    geom_sf(data=land_layer,fill=c("grey90"), col="black")+
    annotation_scale(location="br")+
    theme_bw()+
    ggtitle("Fin Whale")+
    coord_sf(xlim = c(longmin, longmax), ylim = c(latmin, latmax))+
    labs(x=expression(paste("Longitude ",degree,"W",sep="")),
         y=expression(paste("Latitude ",degree,"N",sep="")),
         col="")+
    watermark(show = TRUE, lab = "DFO Internal Use Only")
  
  #Harbour Porpoise
  
  harbour_porpoise_plot <- ggplot()+
    geom_sf(data=studyArea, fill="#74ECFB", col="black", size=0.6)+
    geom_sf(data=harbour_porpoise_sf,fill="#F3E73B",col="#F3E73B")+
    geom_sf(data=bound, col = "red")+ # creates US boundary line, 200 nm limit
    geom_sf(data=land_layer,fill=c("grey90"), col="black")+
    annotation_scale(location="br")+
    theme_bw()+
    ggtitle("Harbour Porpoise")+
    coord_sf(xlim = c(longmin, longmax), ylim = c(latmin, latmax))+
    labs(x=expression(paste("Longitude ",degree,"W",sep="")),
         y=expression(paste("Latitude ",degree,"N",sep="")),
         col="")+
    watermark(show = TRUE, lab = "DFO Internal Use Only")
  
  #humpback whale
  
  humpback_whale_plot <- ggplot()+
    geom_sf(data=studyArea, fill="#74ECFB", col="black", size=0.6)+
    geom_sf(data=humpback_whale_sf,fill="#F3E73B",col="#F3E73B")+
    geom_sf(data=bound, col = "red")+ # creates US boundary line, 200 nm limit
    geom_sf(data=land_layer,fill=c("grey90"), col="black")+
    annotation_scale(location="br")+
    theme_bw()+
    ggtitle("Humpback Whale")+
    coord_sf(xlim = c(longmin, longmax), ylim = c(latmin, latmax))+
    labs(x=expression(paste("Longitude ",degree,"W",sep="")),
         y=expression(paste("Latitude ",degree,"N",sep="")),
         col="")+
    watermark(show = TRUE, lab = "DFO Internal Use Only")
  
  #Sei Whale
  
  sei_whale_plot <- ggplot()+
    geom_sf(data=studyArea, fill="#74ECFB", col="black", size=0.6)+
    geom_sf(data=sei_whale_sf,fill="#F3E73B",col="#F3E73B")+
    geom_sf(data=bound, col = "red")+ # creates US boundary line, 200 nm limit
    geom_sf(data=land_layer,fill=c("grey90"), col="black")+
    annotation_scale(location="br")+
    theme_bw()+
    ggtitle("Sei Whale")+
    coord_sf(xlim = c(longmin, longmax), ylim = c(latmin, latmax))+
    labs(x=expression(paste("Longitude ",degree,"W",sep="")),
         y=expression(paste("Latitude ",degree,"N",sep="")),
         col="")+
    watermark(show = TRUE, lab = "DFO Internal Use Only")
  
  #Arrange all 4 cetaceans into grid
  gridExtra::grid.arrange(fin_whale_plot, harbour_porpoise_plot, humpback_whale_plot,
               sei_whale_plot,
               bottom = expression(paste("Longitude ",degree,"N",sep="")),
               left = expression(paste("Latitude ",degree,"N",sep="")),
               nrow = 2)
}
 
 
# Blue whale important habitat
 
plot_bw_hab <- function(Blue_Whale_sf, studyArea, land_layer, bound) {
  
  ggplot()+
    geom_sf(data=studyArea, fill="#74ECFB", col="black", size=0.6)+
    geom_sf(data=Blue_Whale_sf,aes(fill=Activity), col="black")+
    scale_fill_manual(values=c("#195B01","#1EA31E","chartreuse2","#C5FDC5","blue1"))+
    geom_sf(data=bound, col = "red")+ # creates US boundary line, 200 nm limit
    geom_sf(data=land_layer,fill=c("grey90"), col="black")+
    annotation_scale(location="br")+
    theme_bw()+
    theme(legend.position = "none")+
    coord_sf(xlim = c(-71, -52), ylim = c(41, 51))+
    labs(x=expression(paste("Longitude ",degree,"W",sep="")),
         y=expression(paste("Latitude ",degree,"N",sep="")),
         col="")+
    watermark(show = TRUE, lab = "DFO Internal Use Only")
  
}

# Blue whale important habitat zoom
 
plot_bw_hab_zoom <- function(Blue_Whale_sf, studyArea, land_layer, buf, bound) {
  
  # buf is in km, and now converted to degrees
  buf=buf/100
  buf_lat=buf*0.72
  #png("pez_and_site.png", width=1616, height=1410)
  
  # bounding box
  bbox=st_bbox(studyArea)
  
  # longitude and latitude limits for the map
  longmin<-(bbox$xmin)-buf
  longmax<-bbox$xmax+buf
  latmin<-bbox$ymin-buf_lat
  latmax<-bbox$ymax+buf_lat
  
  ggplot()+
    geom_sf(data=studyArea, fill="#74ECFB", col="black", size=0.6)+
    geom_sf(data=Blue_Whale_sf,aes(fill=Activity),col="black")+
    scale_fill_manual(values=c("#195B01","#1EA31E","chartreuse2","#C5FDC5","blue1"))+
    geom_sf(data=bound, col = "red")+ # creates US boundary line, 200 nm limit
    geom_sf(data=land_layer,fill=c("grey90"), col="black")+
    annotation_scale(location="br")+
    theme_bw()+
    coord_sf(xlim = c(longmin, longmax), ylim = c(latmin, latmax))+
    labs(x=expression(paste("Longitude ",degree,"W",sep="")),
         y=expression(paste("Latitude ",degree,"N",sep="")),
         col="")+
    watermark(show = TRUE, lab = "DFO Internal Use Only")
  
}

# Northern Bottlenose whale important habitat

plot_NBNW_hab <- function(critHab, studyArea, land_layer, bound) {
  
  ggplot()+
    geom_sf(data=studyArea, fill="#74ECFB", col="black", size=0.6)+
    geom_sf(data=critHab,aes(fill='green'),col="black")+
    geom_sf(data=bound, col = "red")+ # creates US boundary line, 200 nm limit
    geom_sf(data=land_layer,fill=c("grey90"), col="black")+
    annotation_scale(location="br")+
    theme_bw()+
    theme(legend.position = "none")+
    coord_sf(xlim = c(-71, -52), ylim = c(41, 51))+
    labs(x=expression(paste("Longitude ",degree,"W",sep="")),
         y=expression(paste("Latitude ",degree,"N",sep="")),
         col="")+
    watermark(show = TRUE, lab = "DFO Internal Use Only")
  
}

# Northern Bottlenose whale important habitat zoom

plot_NBNW_hab_zoom <- function(critHab, studyArea, land_layer, buf, bound) {
  
  # buf is in km, and now converted to degrees
  buf=buf/100
  buf_lat=buf*0.72
  #png("pez_and_site.png", width=1616, height=1410)
  
  # bounding box
  bbox=st_bbox(studyArea)
  
  # longitude and latitude limits for the map
  longmin<-(bbox$xmin)-buf
  longmax<-bbox$xmax+buf
  latmin<-bbox$ymin-buf_lat
  latmax<-bbox$ymax+buf_lat
  
  ggplot()+
    geom_sf(data=studyArea, fill="#74ECFB", col="black", size=0.6)+
    geom_sf(data=critHab,aes(fill='green'),col="black")+
    geom_sf(data=bound, col = "red")+ # creates US boundary line, 200 nm limit
    geom_sf(data=land_layer,fill=c("grey90"), col="black")+
    annotation_scale(location="br")+
    theme_bw()+
    coord_sf(xlim = c(longmin, longmax), ylim = c(latmin, latmax))+
    labs(x=expression(paste("Longitude ",degree,"W",sep="")),
         y=expression(paste("Latitude ",degree,"N",sep="")),
         col="")+
    watermark(show = TRUE, lab = "DFO Internal Use Only")
  
}

########## Habitat Section ##########

# Rockweed presence
plot_rockweed<-function(rockweed_sf, studyArea, land_layer, buf, bound) {
  
  # buf is in km, and now converted to degrees
  buf=buf/100
  buf_lat=buf*0.72
  #png("pez_and_site.png", width=1616, height=1410)
  
  # bounding box
  bbox=st_bbox(studyArea)
  
  # longitude and latitude limits for the map
  longmin<-(bbox$xmin)-buf
  longmax<-bbox$xmax+buf
  latmin<-bbox$ymin-buf_lat
  latmax<-bbox$ymax+buf_lat
  
  # define legends
  rockweed_sf$RWP[which(rockweed_sf$RWP=="1")]= "Present"
  rockweed_sf$RWP[which(rockweed_sf$RWP=="2")]= "Likely Present"
  rockweed_sf$RWP[which(rockweed_sf$RWP=="5")]= "Unknown"
  
  ggplot()+
    geom_sf(data=rockweed_sf, aes(fill=RWP), lwd=0)+
    scale_fill_manual(values=c("orange", "darkgreen", "blue"))+
    geom_sf(data=bound, col = "darkgrey", linetype = "dashed", size = 1.1) + # creates US boundary line, 200 nm limit
    geom_sf(data=land_layer,fill=c("grey90"), col="black")+
    geom_sf(data=studyArea, fill=NA, col="red", size=1)+
    geom_sf(data=site,fill="yellow",col="black", size=0.6)+
    annotation_scale(location="br")+
    theme_bw()+
    theme(legend.title = element_blank())+
    coord_sf(xlim = c(longmin, longmax), ylim = c(latmin, latmax))+
    labs(x=expression(paste("Longitude ",degree,"W",sep="")),
         y=expression(paste("Latitude ",degree,"N",sep="")),
         col="")+
    watermark(show = TRUE, lab = "DFO Internal Use Only")
  
}

########## Spatial Planning Section ##########

# EBSA
 
plot_EBSA<-function(EBSA_sf, studyArea, land_layer, buf, bound) {
  
  # buf is in km, and now converted to degrees
  buf=buf/100
  buf_lat=buf*0.72
  #png("pez_and_site.png", width=1616, height=1410)
  
  # bounding box
  bbox=st_bbox(studyArea)
  
  # longitude and latitude limits for the map
  longmin<-(bbox$xmin)-buf
  longmax<-bbox$xmax+buf
  latmin<-bbox$ymin-buf_lat
  latmax<-bbox$ymax+buf_lat
  
  ggplot()+
    geom_sf(data=studyArea, fill="#74ECFB", col="black", size=0.6)+
    geom_sf(data=EBSA_sf, fill="plum",col="black")+
    geom_sf(data=bound, col = "red")+ # creates US boundary line, 200 nm limit
    geom_sf(data=land_layer,fill=c("grey90"), col="black")+
    geom_sf(data=site,fill="yellow",col="black", size=0.6)+
    annotation_scale(location="br")+
    theme_bw()+
    coord_sf(xlim = c(longmin, longmax), ylim = c(latmin, latmax))+
    labs(x=expression(paste("Longitude ",degree,"W",sep="")),
         y=expression(paste("Latitude ",degree,"N",sep="")),
         col="")+
    watermark(show = TRUE, lab = "DFO Internal Use Only")
  
}
 
