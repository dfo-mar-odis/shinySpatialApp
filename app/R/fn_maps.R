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
                          bottom = expression(paste("Longitude ", degree, "W", sep="")),
                          left = expression(paste("Latitude ", degree, "N", sep="")),
                          nrow = 2)
}


# Rockweed stats
rockweedStats<- function(rockweed_sf, studyArea) {

  # clip rockweed to study area
  rw = sf::st_crop(sf::st_make_valid(rockweed_sf), studyArea)
  rw$area = sf::st_area(rw) # add column with areas of the polygons

  # make a table, sum the areas for different presences
  noRecords = as.data.frame(table(rw$RWP))
  noRecords$Var1 = as.numeric(noRecords$Var1)
  totalArea = aggregate(as.numeric(rw$area), list(rw$RWP), sum)
  stats = merge(noRecords, totalArea, by.x="Var1", by.y="Group.1")
  stats = rbind(stats, colSums(stats))
  names(stats) = c("RWP", "noPolygons", "Area_m2")
  stats$Category = ""
  stats$Category[stats$RWP==1] = "Rockweed present"
  stats$Category[stats$RWP==2] = "Rockweed likely present"
  stats$Category[stats$RWP==5] = "Unknown vegetation"
  stats$Category[stats$RWP==0] = "Rockweed not present"
  stats$Category[nrow(stats)] = "Total intertidal vegetation"

  stats$Area_km2 = round(stats$Area_m2 / 1000) / 1000
  stats = stats[, c("Category", "noPolygons", "Area_km2")]

  return(stats)

}


whale_ggplot <- function(whale_sf, bound, landLayer, studyArea, plotTitle, plotBbox) {

  rawPlot <- ggplot() +
    geom_sf(data=whale_sf, fill="#F3E73B", col="#F3E73B") +
    geom_sf(data=bound, col="darkgrey", linetype="dashed", size=1.1) +
    geom_sf(data=landLayer, fill=c("grey90"), col="black") +
    geom_sf(data=studyArea, fill=NA, col="red", size=1) + 
    ggtitle(plotTitle) 
  
  outPlot <- format_ggplot(rawPlot, plotBbox)
  
  return(outPlot)
}


# ----------------MARFIS ISDB FUNCTIONS -------------------

#
# INPUTS:
# baseGgplot: background ggplot object for all plots with land, axis, etc.  regionMap/areaMap
# data_sf: data to plot
# speciesCodeList: List of species codes indicating which species to plot
# marfis: boolean indicating whether the data is from marfis or isdb
#
#
plot_marfis_grid<-function(baseGgplot, data_sf, speciesCodeList, marfis) {
  
  cleanData <- clean_isdb_marfis_sf(data_sf, speciesCodeList, marfis)
  
  # make all of the individual plots, get rid of any empty plots
  plotList <- lapply(cleanData$specList, plot_isdb_marfis, ggplotIn=baseGgplot, 
                     data_sf=cleanData$data_sf, maxCnt=cleanData$scaleLim)
  plotList <- plotList[!sapply(plotList, is.null)]
  
  
  # plot a 2x2 grid for each four plots:
  chunkSize <- 4
  plotChunks <- split(plotList, ceiling(seq_along(plotList) / chunkSize))
  # use for loop instead of lapply to suppress index output in the rmd
  outPlot = baseGgplot +
    scale_fill_viridis_c(option = "plasma", limits=c(0, cleanData$scaleLim)) + 
    theme(legend.direction = "horizontal",
          legend.position = "bottom") +
    guides(fill=guide_colorbar(title="Estimated combined weight",
                               title.position="top", title.hjust = 0.5))
  #for (i in 1:length(plotList)) {
  #  outPlot = outPlot + plotList[[i]]$layers[[7]]
  #}
  for (chunk in plotChunks) {
    plot(grid_plotter(chunk))
  }
  plot(outPlot)
}


# INPUTS
# data_sf: an sf object, typically striaght from the data preporcessing stage. Should have the original marfis/isdb column names
# specNumList: list of numerical marifs/isdb species codes
# marfis: boolean indicating whether data is marfis or isdb
#
# OUTPUTS: outlist containing:
# data_sf: an sf object with the species common names as column headers for each of the input species codes
# specList: specNumList converted into commmon names
# scaleLim: the maximum value in data_sf, used to coordinate heatmap scales.
#
#
clean_isdb_marfis_sf <- function(data_sf, specNumList, marfis=FALSE){
  
  # convert numeric list to list of col names:
  if (marfis) {
    fullColNames <- marfis_to_col(specNumList)
  }
  else {
    fullColNames <- isdb_to_col(specNumList)
  }
  
  # only select columns asked for that have data:
  data_sf <- dplyr::select(data_sf, any_of(fullColNames))
  noGeom <- sf::st_drop_geometry(data_sf)
  noGeom <- dplyr::select(noGeom, which(colSums(noGeom) > 0))
  
  scaleLim <- max(noGeom)
  colNamesUsed <- names(noGeom)
  data_sf <- dplyr::select(data_sf, all_of(colNamesUsed))

  specNames <-get_spec_names(colNamesUsed, marfis)
  names(data_sf) <- append(specNames, "geometry")
  
  
  outList <- list(data_sf = data_sf,
                  specList = specNames,
                  scaleLim = scaleLim)
  return(outList)
}




# helper function, 
# converts a list of raw marfis/isdb column names to the common names of the species
get_spec_names <- function(colNames, marfis=FALSE) {
  if (marfis) {
    attrNums <- col_to_marfis(colNames)
    lookupTable <- MARFISSPECIESCODES
    lookupCol <- "SPECIES_CODE"
    cnameCol <- "COMMONNAME"
  }
  else {
    attrNums <- col_to_isdb(colNames)
    lookupTable <- ISSPECIESCODES
    lookupCol <- "SPECCD_ID"
    cnameCol <- "Common Name"
    
  }
  specNames <- filter(lookupTable, get(lookupCol) %in% attrNums)[[cnameCol]]
  return(specNames)
}


# adds marfis or isdb data for a given species to an input ggplot. 
plot_isdb_marfis <- function(colName, ggplotIn, data_sf, maxCnt) {
  
  if (colName %in% names(data_sf)) {
    plot_sf <- dplyr::filter(data_sf, (get(colName) > 0))
    if (nrow(plot_sf) > 0) {
      outMap <- ggplotIn +
        geom_sf(data=plot_sf, aes(fill=get(colName)), color = NA) + 
        scale_fill_viridis_c(option = "plasma", limits=c(0, maxCnt)) + 
        theme(legend.position = "none",
              legend.direction = "horizontal") +
        guides(fill=guide_colorbar(title="Estimated combined weight",
                                   title.position="top", title.hjust = 0.5)) +
        theme(axis.title.x=element_blank(),
              axis.title.y=element_blank())+
        labs(subtitle = colName) 
      
      return(outMap)
    } # end of data results in area check
    else {
      return(NULL)
    }
  } # end of column name in data check
  else {
    return(NULL)
  }
}

# plots an input list of plots onto a cowplot grid.
grid_plotter <- function(plotList) {
  legend <- cowplot::get_legend(plotList[[1]] + theme(legend.position = c(0.2, 0.8)))
  p_grid <- cowplot::plot_grid(plotlist = plotList)
  cowplot::plot_grid(p_grid,
                     legend, 
                     ncol = 1, 
                     rel_heights = c(2, 0.3),
                     scale=0.9) +
    cowplot::draw_label(expression(paste("Longitude ",degree,"W",sep="")),
                        x=0.5, y= 0, vjust=-4, angle= 0) +
    cowplot::draw_label(expression(paste("Latitude ",degree,"N",sep="")),
                        x=0, y=0.5, vjust= 3, hjust=-0.1, angle=90)
}


marfis_to_col <- function(marfisNum){
  fullColNames <- paste("X", marfisNum, "_SU", sep = "")
  return(fullColNames)
}

isdb_to_col <- function(isdbNum){
  fullColNames <- paste("EST_COMBINED_WT_", isdbNum, sep = "")
  return(fullColNames)
}

col_to_marfis <- function(marfisCol){
  marfisNum <- gsub("_SU", "", gsub("X", "", marfisCol))
  return(marfisNum)
}

col_to_isdb <- function(isdbCol){
  isdbNum <- gsub("EST_COMBINED_WT_", "", isdbCol)
  return(isdbNum)
}




