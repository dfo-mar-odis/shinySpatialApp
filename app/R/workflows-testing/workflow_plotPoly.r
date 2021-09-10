# WORKFLOW FOR RMD
# 
# For polygons, the only thing to change is the polygon dataset (poly_sf) 
# and attribute to plot ("NONE" if no attribute)
# The rest of the lines stay the same. 

poly_sf <-  BlueWhale_ImpHab_sf #EBSA_sf #NBNW_ImpHab_sf  #leatherback_sf #ClippedCritHab_sf #rockweed_sf
attribute <- "Activity"           #"NONE"         #"NONE"            #"RWP"


# THIS LINE WOULD BE THE SAME FOR ALL POLYGONS
# intersect polyData with 3 polygons (region, map area, and study box)
# master_intersect function is stored in fn_intersect_operations.R
clipped <- master_intersect(poly_sf, mapDataList, getRegion=TRUE)
# outputs: clipped$regionData, clipped$mapData, and clipped$studyData

# PLOT AREA MAP (using clipped$mapData)
if (!is.null(clipped$mapData)){
  plot_rr_sf(areaMap, clipped$mapData, attribute) 
}

  
# PLOT REGION MAP (if needed, using clipped$regionData)
if (!is.null(clipped$regionData)){
  plot_rr_sf(regionMap, clipped$regionData, attribute) 
}  
  
