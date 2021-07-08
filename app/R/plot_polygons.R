
# Function for plotting polygons for the reproducible report.
#
# Inputs:
# 1. baseMap = map object, either areaMap or regionMap
# 2. mapBox: bbox object, representing bounding box of the map, bboxMap (for the areaMap) or regionBox (for regionMap)  
# 3. studyBox_geom: geometry polygon with the area of interest (defined through shiny app and in the intro)
# 4. polyData: polygon data to be plotted 
#    (pre-clipped to map area with the poly_intersect function, using bboxMap, or regionBox)
# 5. attribute: attribute in the polygon data to be plotted (column name as a string, e.g. "Activity", or "Rockweed"; 
#               this attribute name will appear in the legend. For single color polygons use attribute="NONE".
#    
# 
# Examples of use:
#
#   To plot on region map: plot_polygons(regionMap,regionBox,studyBox_geom, data$regionPoly,attribute="NONE")
#                          (only parameters to change are data$regionPoly and attribute)
#
#   To plot area map: plot_polygons(areaMap,bboxMap,studyBox_geom, data$regionPoly,attribute="NONE")
#                          (only parameters to change are data$regionPoly and attribute)
#
# Created by Gordana Lazin, July 2, 2021 for reproducible reporting project


plot_polygons <- function(baseMap, mapBox, studyBox_geom, polyData, attribute){
  
  clr="black" #color for outlining polygons
  
  # this is part specific for rockweed, I included it in plotting for now
  # Otherwise, it can be included in intersect function, or in pre-processing
  if (attribute == "RWP") {
    
    # replace codes with words
    polyData$Rockweed=""
    polyData$Rockweed[which(polyData$RWP==1)]="1-Present"
    polyData$Rockweed[which(polyData$RWP==2)]="2-Likely Present"
    polyData$Rockweed[which(polyData$RWP==5)]="5-Unknown"
    polyData$Rockweed[which(polyData$RWP==0)]="Not Present"
    
    attribute="Rockweed"
    clr=NA
  }
  
  # axis limits to the plot
  axLim=ggplot2::coord_sf(xlim=c(mapBox["xmin"], mapBox["xmax"]),ylim=c(mapBox["ymin"], mapBox["ymax"]),expand=FALSE)
  
  # color-blind options for the legend
  legendColor=c("#009E73", "#E69F00", "#0072B2", "#CC79A7","#F0E442","#D55E00","#56B4E9","#999999")
  
  
  # there are two types of plots: 
  # Case 1: all polygons are one color, no legend,
  # case 2: polygons are colored based on the "attribute" column, legend is included
  
  if (toupper(attribute)=="NONE"){ # Case 1: plotting all polygons in one color
    
    polyPlot <-geom_sf(data=polyData,fill="#56B4E9",col=clr)
    polyFill=NULL
    
  }else{ # Case 2: plotting polygons in different colors based on "attribute" column in the data
    
    polyPlot<-geom_sf(data=polyData, aes(fill=!!sym(attribute)), colour=clr)
    polyFill <-scale_fill_manual(values=legendColor)
  }
  
 
  # this is how general plotting function should look like
    
    polyMap <- baseMap+
      polyPlot +
      polyFill +
      studyBox_geom +
      axLim
    
    return(polyMap)
  
}