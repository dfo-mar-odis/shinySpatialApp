# script to walkthrough hex_data handling:

marf2015_demo <- readRDS(here::here("hex_data/marf2015_DemoHex_sp.RDS"))
hexGrid <- marf2015_demo[["Grid2Min"]]
hex_sf <- sf::st_as_sf(hexGrid)

spec_df <- dplyr::select(hex_sf, -ORD_gr, -HEXID)
spec_df <- sf::st_drop_geometry(spec_df)
hex_sf$count <- rowSums(spec_df != 0) / 2
hex_sf <- dplyr::filter(hex_sf, count > 0)

hex_sf$count <- as.factor(hex_sf$count)
poly_sf <- hex_sf
attribute <- "count"
clipped <- master_intersect(hex_sf, region, hex_sf, bboxMap)
plot_polygons(areaMap, bboxMap, studyBox_geom, clipped$mapData, attribute)


plot_spec <- function(data_sf, specCol) {
  plot_sf <- dplyr::filter(data_sf, get(specCol) > 0)
  
  # use cut function here instead of log.
  # alternatively use continuous scale colour bar
  plot_sf$attr <- as.factor(log(plot_sf[[specCol]], base=10) %/% 1)
  attribute <- "attr"
  clipped <- master_intersect(plot_sf, region, studyArea, bboxMap)
  plot_polygons(areaMap, bboxMap, studyBox_geom, clipped$mapData, attribute)
  
}
