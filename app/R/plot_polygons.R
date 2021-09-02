
# Function for plotting sf data for the reproducible report.
#
# Inputs:
# 1. baseMap = map object, either areaMap or regionMap
# 2. data_sf: sf data to be plotted, can contain either point or polygon data
#    (ideally, pre-clipped to map area with the master_intersect function, using bboxMap, or regionBox)
# 3. attribute: column name of factor data in data_sf. 
#               this attribute name will appear in the legend. For single color polygons leave blank
# 4. legendName: string, sets the name of the legend for cases where the attribute is not appropriate. Defaults to
#                the attribute.  
# 5. legendColours: list of colours used to set the scale.  eg. WhaleCol. Only used for point data
#    
# 
# Created by Quentin Stoyel, September 2, 2021 for reproducible reporting project

plot_rr_sf <- function(baseMap, data_sf, attribute=NULL, legendName="", legendColours=NULL) {
  if (inherits(sf::st_geometry(data_sf), "sfc_POINT")) {
    outPlot <- plot_points(baseMap, data_sf, attribute=attribute, legendName=legendName, legendColours=legendColours)
  } else if (inherits(sf::st_geometry(data_sf), c("sfc_POLYGON", "sfc_MULTIPOLYGON"))) {
    outPlot <- plot_polygons(baseMap, data_sf, attribute=attribute, legendName=legendName)
  }
  else {
    stop("Geometry type not supported")
  }
  return(outPlot)
}



# helper function, extracts the scale bar from either the areaMap or regionMap
get_scale_bar_layer <- function(inPlot) {
  scaleBarLayer <- lapply(inPlot$layers, function(inLayer) if("GeomScaleBar" %in% class(inLayer$geom)) inLayer else NULL)
  scaleBarLayer <- scaleBarLayer[!sapply(scaleBarLayer, is.null)]
  return(scaleBarLayer)
}


# helper function, extracts the study_box_layer from either the areaMap or regionMap
# selection criteria is based off of colour, use with care.
get_study_box_layer <- function(inPlot) {
  studyBoxLayer <- lapply(inPlot$layers, function(inLayer) if("red" %in% c(inLayer$aes_params$colour)) inLayer else NULL)
  studyBoxLayer <- studyBoxLayer[!sapply(studyBoxLayer, is.null)]
  return(studyBoxLayer)
}



# Function for plotting point data for the reproducible report.
#
# Inputs:
# 1. baseMap = map object, either areaMap or regionMap
# 2. data_sf: sf data to be plotted 
#    (ideally, pre-clipped to map area with the master_intersect function, using bboxMap, or regionBox)
# 3. attribute: column name of factor data in data_sf. 
#               this attribute name will appear in the legend. For single color polygons leave blank
# 4. legendName: string, sets the name of the legend for cases where the attribute is not appropriate. Defaults to
#                the attribute.  
# 5. legendColours: list of colours used to set the scale.  eg. WhaleCol
#    
# 
# Created by Quentin Stoyel, September 2, 2021 for reproducible reporting project

plot_points <- function(baseMap, data_sf, attribute=NULL, legendName="", legendColours=NULL) {
  
  # extract scaleBar layer to ensure it plots over polygons/study area box
  scaleBarLayer = get_scale_bar_layer(baseMap)
  studyBoxLayer = get_study_box_layer(baseMap)
  
  # axis limits based on baseMap
  axLim = ggplot2::coord_sf(xlim=baseMap$coordinates$limits$x, 
                            ylim=baseMap$coordinates$limits$y, expand=FALSE) 

  if (is.null(attribute)) {
    # just plot raw data
    dataLayer <- geom_sf(data = data_sf, size = 2, shape = 20) 
    legendLayer <- NULL
  } else {
    dataLayer <- geom_sf(data = data_sf, aes(color=!!sym(attribute)), size = 2, shape = 20)
    if (!is.null(legendColours)){
      legendLayer <- ggplot2::scale_colour_manual(values=legendColours, name=legendName)  
    }
  }
    
  pointMap <- baseMap +
    dataLayer +
    legendLayer +
    axLim +
    studyBoxLayer +
    scaleBarLayer
  
  return(pointMap) 
}


# Function for plotting polygons for the reproducible report.
#
# Inputs:
# 1. baseMap = map object, either areaMap or regionMap
# 2. polyData: polygon data to be plotted 
#    (pre-clipped to map area with the master_intersect function, using bboxMap, or regionBox)
# 3. attribute: attribute in the polygon data to be plotted (column name as a string, e.g. "Activity", or "Rockweed"; 
#               this attribute name will appear in the legend. For single color polygons use attribute="NONE".
# 4. legendName: string, sets the name of the legend for cases where the attribute is not appropriate. Defaults to
#                the attribute.  
#    
# 
# Examples of use:
#
#   To plot on region map: plot_polygons(regionMap, regionBox, studyBox_geom, data$regionPoly, attribute="NONE")
#                          (only parameters to change are data$regionPoly and attribute)
#
#   To plot area map: plot_polygons(areaMap, bboxMap, studyBox_geom, data$regionPoly, attribute="NONE")
#                          (only parameters to change are data$regionPoly and attribute)
#
# Created by Gordana Lazin, July 2, 2021 for reproducible reporting project


plot_polygons <- function(baseMap, polyData, attribute, legendName=attribute) {
  
  scaleBarLayer = get_scale_bar_layer(baseMap)
  studyBoxLayer = get_study_box_layer(baseMap)
  
  clr = "black" #color for outlining polygons
  
  # this is part specific for rockweed, I included it in plotting for now
  # Otherwise, it can be included in intersect function, or in pre-processing
  if (attribute == "RWP") {
    
    # replace codes with words
    polyData$Rockweed = ""
    polyData$Rockweed[which(polyData$RWP==1)] = "1-Present"
    polyData$Rockweed[which(polyData$RWP==2)] = "2-Likely Present"
    polyData$Rockweed[which(polyData$RWP==5)] = "5-Unknown"
    polyData$Rockweed[which(polyData$RWP==0)] = "Not Present"
    
    attribute = "Rockweed"
    clr = NA
  }
  
  # axis limits to the plot
  axLim = ggplot2::coord_sf(xlim=baseMap$coordinates$limits$x, 
                            ylim=baseMap$coordinates$limits$y, expand=FALSE) 
  
  # color-blind options for the legend
  legendColor=c("#009E73", "#E69F00", "#0072B2", "#CC79A7", "#F0E442", 
                "#D55E00", "#56B4E9","#999999", "black")
  
  
  # there are two types of plots: 
  # Case 1: all polygons are one color, no legend,
  # case 2: polygons are colored based on the "attribute" column, legend is included
  
  if (toupper(attribute) == "NONE") { # Case 1: plotting all polygons in one color
    
    polyPlot <- geom_sf(data=polyData, fill="#56B4E9", col=clr)
    polyFill <- NULL
    
  } else { # Case 2: plotting polygons in different colors based on "attribute" column in the data
    
    polyPlot <- geom_sf(data=polyData, aes(fill=!!sym(attribute)), colour=clr)
    polyFill <- scale_fill_manual(values=legendColor, name=legendName) 
    
  }
 
  # this is how general plotting function should look like
    
    polyMap <- baseMap +
      polyPlot +
      polyFill +
      studyBoxLayer +
      axLim +
      scaleBarLayer
    
    return(polyMap)
  
}