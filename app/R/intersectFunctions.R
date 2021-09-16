# The functions in this file are used to clip various data sources (e.g. MARFIS, ISDB, etc.)
# with the studyArea selected and summarize the data for tables within the final document.
#
# 1. master_intersect() - clips POINT and POLYGON data to the extent of the studyArea, previously main_intersect()
#
# Written by Philip Greyson for Reproducible Reporting project, May/2021



##### - master_intersect function ##################################
# This function clips various data sources (e.g. MARFIS, ISDB, etc.)
# to the extent of the studyArea and the map bounding box.
# The map bounding box is created by the area_map() function
#
# Inputs:
# 1. data_sf: an input polygon vector file
# 2. region: a spatial file of the region
# 3. studyArea: polygon of the study area (sf object, defined by the user in the shiny app)
# 4. mapBbox: Coordinates of the map bounding box exported from area_map() function (bboxMap)
#
# Outputs: list containing 4 items
# 1. studyData: the full dataset from clipping data_sf by the studyArea
# 2. mapData: the full dataset from clipping data_sf by the Bounding box
# 3. regionData: the full dataset from clipping data_sf by the region
# 4. mapPoints: Unique collection of points to be plotted on a map.


master_intersect <- function(data_sf, mapDataList, getRegion=FALSE, ...) {

  # check that data_sf is an accepted format:
  if (!inherits(sf::st_geometry(data_sf), c("sfc_POINT", 
                                            "sfc_POLYGON", 
                                            "sfc_MULTIPOLYGON",
                                            "sfc_GEOMETRY")))
  {
    outList <- list(regionData = NULL,
                    studyData = NULL, 
                    mapData = NULL, 
                    mapPoints = NULL)
    return(outList)
  }
  
  # convert bbox to sf object representing the map
  mapArea <- sf::st_as_sfc(mapDataList$bboxMap)
  
  # Crop data
  if (getRegion) {
    regionData <- sf::st_crop(data_sf, mapDataList$region)  
    if (nrow(regionData) == 0) {regionData <- NULL}
  } else {
    regionData <- NULL
  }
  
  mapData <- sf::st_crop(data_sf, mapArea)
  studyData <- sf::st_crop(mapData, mapDataList$studyArea)
  
  # if there is no intersect with the box, set return to NULL
  if (nrow(mapData) == 0) {mapData <- NULL}
  
  if (nrow(studyData) > 0) {
    # if there is point data in the study area, drop uneeded columns and 
    # duplicate geometries
    # for RV the ELAT and ELONG fields are necessary as well
    if (inherits(sf::st_geometry(data_sf), "sfc_POINT")) {
      if ("ELAT" %in% colnames(mapData)) {
        mapPoints <- dplyr::select(mapData, ELAT, ELONG, geometry)
      } 
      else {
        mapPoints <- dplyr::select(mapData, geometry)
      }
      # remove redundant geometries and set lat/long columns:
      mapPoints <- unique(mapPoints)
      mapPoints <- sfcoords_as_cols(mapPoints)
    } # end of test for point geometry
    else {
      mapPoints <- NULL
    }
  } # end of test for zero samples
  else {
    studyData <- NULL
    mapPoints <- NULL
  }
  outList <- list(regionData = regionData,
                  studyData = studyData, 
                  mapData = mapData, 
                  mapPoints = mapPoints)
  return(outList)
}

  
##### - END master_intersect function ##################################


##### - raster_intersect function ##################################
# This function clips various raster data sources (e.g. SDM output, etc.)
# to the extent of the region, the studyArea, and the map bounding box.
# The map bounding box is created by the area_map() function
#
# Inputs:
# 1. datafile: an input raster file
# 2. region: a spatial file of the region
# 3. studyArea: polygon of the study area (sf object, defined by the user in the shiny app)
# 4. mapBbox: Coordinates of the map bounding box exported from area_map() function
#
# Outputs: list containing 3 items
# 1. studyRas: the full dataset from clipping the datafile by the studyArea
# 2. mapRas: the full dataset from clipping the datafile by the Bounding box
# 3. regionRas: the full dataset from clipping the datafile by the region
raster_intersect <- function(datafile, region, studyArea, mapBbox, ...) {

  # convert Bbox to sf object
  tmpBbox <- st_as_sfc(mapBbox)
  mapArea <- st_as_sf(tmpBbox)
  # Crop Data
  # with raster datasets it's necesssary to comnbine the
  # crop and mask functions
  regionData <- crop(datafile, region)
  regionData <- raster::mask(regionData, region)
  mapData <- crop(regionData, mapArea)
  mapData <- mask(mapData, mapArea)
  studyData <- crop(mapData, studyArea)
  studyData <- mask(studyData, studyArea)

  # if there are no raster cells found in the studyArea, exit the function
  if (nrow(studyData) > 0) {
    outList <- list(studyRas = studyData, mapRas = mapData, regionRas = regionData)
    return(outList)

  } # end of test for zero samples
  else {
    return()
  }
}
##### - END raster_intersect function ##################################
