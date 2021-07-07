# WORKFLOW FOR RMD
# 
# For polygons, the only thing to change is the polygon dataset (poly_sf) 
# and attribute to plot ("NONE" if no attribute)
# The rest of the lines stay the same. 

poly_sf= leatherback_sf #EBSA_sf #NBNW_ImpHab_sf #BlueWhale_ImpHab_sf  #leatherback_sf #ClippedCritHab_sf #rockweed_sf
attribute="NONE" #"NONE"         #"Activity"           #"NONE"         #"NONE"            #"RWP"


# THIS LINE WOULD BE THE SAME FOR ALL POLYGONS
# intersect polyData with 3 polygons (region, map area, and study box)
# poly_intersect function is stored in fn_intersect_operations.R
clipped=poly_intersect(poly_sf,Region,studyArea, bboxMap)
# outputs: clipped$regionPoly, clipped$mapPoly, and clipped$studyPoly

# PLOT AREA MAP (using clipped$mapPoly)
if (!is.null(clipped$mapPoly)){
  plot_polygons(areaMap, bboxMap, studyBox_geom, clipped$mapPoly,attribute) 
}

  
# PLOT REGION MAP (if needed, using clipped$regionPoly)
if (!is.null(clipped$regionPoly)){
  plot_polygons(regionMap,regionBox,studyBox_geom, clipped$regionPoly,attribute) 
}  
  
