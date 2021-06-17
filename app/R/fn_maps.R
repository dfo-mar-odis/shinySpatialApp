# #site map

site_map <- function(studyArea,site,land_layer,buf, bound) {
  
  # buf is in km, and now converted to degrees
  buf=buf/100
  
  # bounding box
  bbox=sf::st_bbox(studyArea)
  
  # longitude and latitude limits for the map
  longmin<-(bbox$xmin)-buf
  longmax<-bbox$xmax+buf
  latmin<-bbox$ymin-buf
  latmax<-bbox$ymax+buf
  
  ggplot()+
    geom_sf(data=site,fill="yellow",col="black", size=0.6)+
    geom_sf(data=bound, col = "darkgrey", linetype = "dashed", size = 1.1) +  # creates US boundary line, 200 nm limit
    geom_sf(data=land_layer,fill=c("lightgrey"), col="black", size=0.7)+
    watermark(show = TRUE, lab = "DFO Internal Use Only")+
    geom_sf(data=studyArea, fill=NA, col="red", size=1)+
    annotation_scale(location="bl")+
    theme_bw()+
    ggplot2::coord_sf(xlim = c(longmin, longmax), ylim = c(latmin, latmax))+
    labs(x="Longitude", y="Latitude", col="")+
    theme(axis.title.y = element_text(size = 13))+
    theme(axis.title.x = element_text(size = 13))
  
}

########## Species At Risk distribution and Critical Habitat data ##########

# critical habitat
plot_crithab <- function(ClippedCritHab_sf, leatherback_sf, studyArea, land_layer, buf, bound) {
  
  # buf is in km, and now converted to degrees
  buf=buf/100
  buf_lat=buf*0.72
  
  # bounding box
  bbox <- sf::st_bbox(studyArea)
  
  # longitude and latitude limits for the map
  longmin <- (bbox$xmin)-buf
  longmax <- bbox$xmax+buf
  latmin <- bbox$ymin-buf_lat
  latmax <- bbox$ymax+buf_lat
  
  ggplot()+
    geom_sf(data=leatherback_sf,fill="lightgreen",col="black")+
    geom_sf(data=ClippedCritHab_sf,fill="red",col="black")+
    geom_sf(data=bound, col = "darkgrey", linetype = "dashed", size = 1.1)+  # creates US boundary line, 200 nm limit
    geom_sf(data=land_layer,fill=c("grey90"), col="black")+
    geom_sf(data=studyArea, fill=NA, col="red", size=1)+
    annotation_scale(location="br")+
    theme_bw()+
    ggplot2::coord_sf(xlim = c(longmin, longmax), ylim = c(latmin, latmax))+
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
  bbox=sf::st_bbox(studyArea)
  
  # longitude and latitude limits for the map
  longmin <- (bbox$xmin)-buf
  longmax <- bbox$xmax+buf
  latmin <- bbox$ymin-buf_lat
  latmax <- bbox$ymax+buf_lat
  
  ggplot()+
    geom_sf(data=sardist_sf,fill="orange", col="black", size=0.6)+    
    geom_sf(data=bound, col = "darkgrey", linetype = "dashed", size = 1.1) +  # creates US boundary line, 200 nm limit
    geom_sf(data=land_layer,fill=c("grey90"), col="black")+
    geom_sf(data=studyArea, fill=NA, col="red", size=1)+
    annotation_scale(location="br")+
    theme_bw()+
    ggplot2::coord_sf(xlim = c(longmin, longmax), ylim = c(latmin, latmax))+
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


# Blue whale important habitat

plot_bw_hab <- function(Blue_Whale_sf, studyArea, land_layer, bound) {
  
  ggplot()+
    geom_sf(data=studyArea, fill=NA, col="red", size=1)+
    geom_sf(data=Blue_Whale_sf,aes(fill=Activity), col="black")+
    scale_fill_manual(values=c("#195B01","#1EA31E","chartreuse2","#C5FDC5","blue1"))+
    geom_sf(data=bound, col = "darkgrey", linetype = "dashed", size = 1.1) + # creates US boundary line, 200 nm limit
    geom_sf(data=land_layer,fill=c("grey90"), col="black")+
    annotation_scale(location="br")+
    theme_bw()+
    theme(legend.position = "none")+
    ggplot2::coord_sf(xlim = c(-71, -52), ylim = c(41, 51))+
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
    geom_sf(data=studyArea, fill=NA, col="red", size=1)+
    geom_sf(data=Blue_Whale_sf,aes(fill=Activity),col="black")+
    scale_fill_manual(values=c("#195B01","#1EA31E","chartreuse2","#C5FDC5","blue1"))+
    geom_sf(data=bound, col = "darkgrey", linetype = "dashed", size = 1.1) +  # creates US boundary line, 200 nm limit
    geom_sf(data=land_layer,fill=c("grey90"), col="black")+
    annotation_scale(location="br")+
    theme_bw()+
    ggplot2::coord_sf(xlim = c(longmin, longmax), ylim = c(latmin, latmax))+
    labs(x=expression(paste("Longitude ",degree,"W",sep="")),
         y=expression(paste("Latitude ",degree,"N",sep="")),
         col="")+
    watermark(show = TRUE, lab = "DFO Internal Use Only")
  
}

# Northern Bottlenose whale important habitat

plot_NBNW_hab <- function(critHab, studyArea, land_layer, bound) {
  
  ggplot()+
    geom_sf(data=studyArea, fill=NA, col="red", size=1)+
    geom_sf(data=critHab,aes(fill='green'),col="black")+
    geom_sf(data=bound, col = "darkgrey", linetype = "dashed", size = 1.1) +  # creates US boundary line, 200 nm limit
    geom_sf(data=land_layer,fill=c("grey90"), col="black")+
    annotation_scale(location="br")+
    theme_bw()+
    theme(legend.position = "none")+
    ggplot2::coord_sf(xlim = c(-71, -52), ylim = c(41, 51))+
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
    geom_sf(data=studyArea, fill=NA, col="red", size=1)+
    # geom_sf(data=critHab,aes(fill='green'),col="black")+
    geom_sf(data=critHab,col="black", fill = "red")+
    geom_sf(data=bound, col = "darkgrey", linetype = "dashed", size = 1.1) +  # creates US boundary line, 200 nm limit
    geom_sf(data=land_layer,fill=c("grey90"), col="black")+
    annotation_scale(location="br")+
    theme_bw()+
    ggplot2::coord_sf(xlim = c(longmin, longmax), ylim = c(latmin, latmax))+
    labs(x=expression(paste("Longitude ",degree,"W",sep="")),
         y=expression(paste("Latitude ",degree,"N",sep="")),
         col="")+
    watermark(show = TRUE, lab = "DFO Internal Use Only")
  
}

########## Habitat Section ##########

# Rockweed presence [by Gordana Lazin]
plot_rockweed <- function(rockweed_sf, areaMap, bboxMap) {
  
  # crop rockweed layer to the map area to speed up plotting
  rockweed=st_crop(st_make_valid(rockweed_sf),bboxMap)
  
  if (dim(rockweed)[1]==0){
    rockweedMap=NULL
  }else{
    
    # define axis limit
    axLim=ggplot2::coord_sf(xlim = c(bboxMap["xmin"], bboxMap["xmax"]), ylim = c(bboxMap["ymin"], bboxMap["ymax"]),expand=FALSE)
    
    # replace codes with words
    rockweed$Rockweed=""
    rockweed$Rockweed[which(rockweed$RWP==1)]="1-Present"
    rockweed$Rockweed[which(rockweed$RWP==2)]="2-Likely Present"
    rockweed$Rockweed[which(rockweed$RWP==5)]="5-Unknown"
    rockweed$Rockweed[which(rockweed$RWP==0)]="Not Present"
    
    rockweedMap <- areaMap +
      geom_sf(data=rockweed, aes(fill=Rockweed), colour=NA)+
      scale_fill_manual(values=c("#009E73", "#E69F00", "#0072B2","#999999"))+
      studyBox_geom +
      axLim
  }
  
  return(rockweedMap)
  
}


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
  
  EBSAmap <- ggplot()+
    geom_sf(data=EBSA_sf, fill="plum",col="black")+
    geom_sf(data=bound, col = "darkgrey", linetype = "dashed", size = 1.1) + # creates US boundary line, 200 nm limit
    geom_sf(data=studyArea, fill=NA, col="red", size=1)+
    geom_sf(data=land_layer,fill=c("grey90"), col="black")+
    geom_sf(data=site,fill="yellow",col="black", size=0.6)+
    annotation_scale(location="br")+
    theme_bw()+
    ggplot2::coord_sf(xlim = c(longmin, longmax), ylim = c(latmin, latmax))+
    labs(x=expression(paste("Longitude ",degree,"W",sep="")),
         y=expression(paste("Latitude ",degree,"N",sep="")),
         col="")+
    watermark(show = TRUE, lab = "DFO Internal Use Only")
  return(EBSAmap)
}

