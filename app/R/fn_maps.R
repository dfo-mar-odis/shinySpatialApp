# #Rasters - Grid of 4 cetacean priority habitat

plot_cetaceans_4grid<-function(fin_whale_sf, harbour_porpoise_sf,
                               humpback_whale_sf, sei_whale_sf, studyArea,
                               landLayer, bufKm, bounds_sf) {
  # buf is in km, and now converted to degrees
  buf <- bufKm / 100
  bufLong <- buf * 2

  # bounding box
  bbox <- st_bbox(studyArea)
  
  bboxBuf <- bbox
  
  bboxBuf["xmin"] <- (bbox$xmin) - bufLong
  bboxBuf["xmax"] <- (bbox$xmax) + bufLong
  bboxBuf["ymin"] <- (bbox$ymin) - buf
  bboxBuf["ymax"] <- (bbox$ymax) + buf
  
  land <- sf::st_crop(landLayer, bboxBuf)
  bound <- sf::st_crop(bounds_sf, bboxBuf)
  
  #Fin Whale
  finWhalePlot <- whale_ggplot(fin_whale_sf, bound, land, studyArea,
                               "Fin Whale", bboxBuf)
 
  #Harbour Porpoise
  harbourPorpoisePlot <- whale_ggplot(harbour_porpoise_sf, bound, land, studyArea,
                                    "Harbour Porpoise", bboxBuf)

  #humpback whale
  humpbackWhalePlot <- whale_ggplot(humpback_whale_sf, bound, land, studyArea,
                                    "Humpback Whale", bboxBuf)
  
  #Sei Whale
  seiWhalePlot <- whale_ggplot(sei_whale_sf, bound, land, studyArea,
                               "Sei Whale", bboxBuf)

  #Arrange all 4 cetaceans into grid
  gridExtra::grid.arrange(finWhalePlot, harbourPorpoisePlot, humpbackWhalePlot,
                          seiWhalePlot,
                          bottom = expression(paste("Longitude ",degree,"N",sep="")),
                          left = expression(paste("Latitude ",degree,"N",sep="")),
                          nrow = 2)
}


# Rockweed stats
rockweedStats<- function(rockweed_sf, studyArea) {

  # clip rockweed to study area
  rw = sf::st_crop(sf::st_make_valid(rockweed_sf), studyArea)
  rw$area = sf::st_area(rw) # add column with areas of the polygons

  # make a table, sum the areas for different presences
  noRecords = as.data.frame(table(rw$status))
  totalArea = aggregate(as.numeric(rw$area), list(rw$status), sum)
  stats = merge(noRecords, totalArea, by.x="Var1", by.y="Group.1")
  
  statusCol <- totalArea$Group.1
  stats <- dplyr::select(stats, c("Freq", "x"))
  stats = rbind(stats, colSums(stats))
  statusCol[nrow(stats)] = "Total intertidal vegetation"
  stats$status <- statusCol
  stats <- dplyr::select(stats, c("status", "Freq", "x"))
  
  names(stats) = c("Status", "noPolygons", "Area_m2")

  stats$Area_km2 = round(stats$Area_m2 / 1000) / 1000
  stats = stats[, c("Status", "noPolygons", "Area_km2")]

  return(stats)

}


whale_ggplot <- function(whale_sf, bound, landLayer, studyArea, plotTitle, plotBbox) {

  rawPlot <- ggplot() +
    geom_sf(data=whale_sf, fill="#F3E73B", col="#F3E73B") +
    geom_sf(data=bound, col="darkgrey", linetype="dashed", size=1.1) +
    geom_sf(data=landLayer, fill=c("lightgrey"), col="lightgrey") +
    geom_sf(data=studyArea, fill=NA, col="red", size=1) + 
    ggtitle(plotTitle) 
  
  outPlot <- format_ggplot(rawPlot, plotBbox)
  
  return(outPlot)
}

