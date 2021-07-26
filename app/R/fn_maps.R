# #Rasters - Grid of 4 cetacean priority habitat

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
    geom_sf(data=fin_whale_sf,fill="#F3E73B",col="#F3E73B")+
    geom_sf(data=bound, col = "darkgrey", linetype = "dashed", size = 1.1) +  # creates US boundary line, 200 nm limit
    geom_sf(data=land_layer,fill=c("grey90"), col="black")+
    geom_sf(data=studyArea, fill=NA, col="red", size=1)+
    annotation_scale(location="br")+
    theme_bw()+
    ggtitle("Fin Whale")+
    ggplot2::coord_sf(xlim = c(longmin, longmax), ylim = c(latmin, latmax))+
    labs(x=expression(paste("Longitude ",degree,"W",sep="")),
         y=expression(paste("Latitude ",degree,"N",sep="")),
         col="")+
    watermark(show = TRUE, lab = "DFO Internal Use Only")
  
  #Harbour Porpoise
  
  harbour_porpoise_plot <- ggplot()+
    geom_sf(data=harbour_porpoise_sf,fill="#F3E73B",col="#F3E73B")+
    geom_sf(data=bound, col = "darkgrey", linetype = "dashed", size = 1.1) +  # creates US boundary line, 200 nm limit
    geom_sf(data=land_layer,fill=c("grey90"), col="black")+
    geom_sf(data=studyArea, fill=NA, col="red", size=1)+
    annotation_scale(location="br")+
    theme_bw()+
    ggtitle("Harbour Porpoise")+
    ggplot2::coord_sf(xlim = c(longmin, longmax), ylim = c(latmin, latmax))+
    labs(x=expression(paste("Longitude ",degree,"W",sep="")),
         y=expression(paste("Latitude ",degree,"N",sep="")),
         col="")+
    watermark(show = TRUE, lab = "DFO Internal Use Only")
  
  #humpback whale
  
  humpback_whale_plot <- ggplot()+
    geom_sf(data=humpback_whale_sf,fill="#F3E73B",col="#F3E73B")+
    geom_sf(data=bound, col = "darkgrey", linetype = "dashed", size = 1.1) +  # creates US boundary line, 200 nm limit
    geom_sf(data=land_layer,fill=c("grey90"), col="black")+
    geom_sf(data=studyArea, fill=NA, col="red", size=1)+
    annotation_scale(location="br")+
    theme_bw()+
    ggtitle("Humpback Whale")+
    ggplot2::coord_sf(xlim = c(longmin, longmax), ylim = c(latmin, latmax))+
    labs(x=expression(paste("Longitude ",degree,"W",sep="")),
         y=expression(paste("Latitude ",degree,"N",sep="")),
         col="")+
    watermark(show = TRUE, lab = "DFO Internal Use Only")
  
  #Sei Whale
  
  sei_whale_plot <- ggplot()+
    geom_sf(data=sei_whale_sf,fill="#F3E73B",col="#F3E73B")+
    geom_sf(data=bound, col = "darkgrey", linetype = "dashed", size = 1.1) +  # creates US boundary line, 200 nm limit
    geom_sf(data=land_layer,fill=c("grey90"), col="black")+
    geom_sf(data=studyArea, fill=NA, col="red", size=1)+
    annotation_scale(location="br")+
    theme_bw()+
    ggtitle("Sei Whale")+
    ggplot2::coord_sf(xlim = c(longmin, longmax), ylim = c(latmin, latmax))+
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


# Rockweed stats
rockweedStats<- function(rockweed_sf, studyArea) {
  
  # clip rockweed to study area
  rw=sf::st_crop(sf::st_make_valid(rockweed_sf),studyArea)
  rw$area=sf::st_area(rw) # add column with areas of the polygons
  
  # make a table, sum the areas for different presences
  noRecords=as.data.frame(table(rw$RWP))
  noRecords$Var1=as.numeric(noRecords$Var1)
  totalArea=aggregate(as.numeric(rw$area), list(rw$RWP), sum)
  stats=merge(noRecords,totalArea, by.x="Var1",by.y="Group.1")
  stats=rbind(stats,colSums(stats))
  names(stats)=c("RWP","noPolygons","Area_m2")
  stats$Category=""
  stats$Category[stats$RWP==1]="Rockweed present"
  stats$Category[stats$RWP==2]="Rockweed likely present"
  stats$Category[stats$RWP==5]="Unknown vegetation"
  stats$Category[stats$RWP==0]="Rockweed not present"
  stats$Category[nrow(stats)]="Total intertidal vegetation"
  
  stats$Area_km2=round(stats$Area_m2/1000)/1000
  stats=stats[,c("Category","noPolygons","Area_km2")]
  
  
  return(stats)
  
}

