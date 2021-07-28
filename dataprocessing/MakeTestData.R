######## MAKE TEST DATA FUNCTIONS ###############
# Functions used to create test data for the application
# Primary function to configure/use is: gen_all_test_data() which will generate
# a .Rdata file with usable test data.  
#
#
# Usage: gen_all_test_data()
# Output file will be saved under app/data/testData.Rdata



##### - get random points ##################################
# This function generates a collection of random points inside the region
# delimited by the bounding box and weighted into the test area boxes.
#
# Inputs:
# 1. mult: how many points should be generated
#
# Outputs:
# 1. out_samp: vector of point geometries with length = 10 * mult 


get_random_points <- function(mult = 10) {
  studyArea1 <- st_read(here::here("app/studyAreaTest/geoms_slc_test.geojson"))#Offshore
  studyArea2 <- st_read(here::here("app/studyAreaTest/geoms_slc_coastal_test.geojson"))#coastal 
  studyArea3 <- st_read(here::here("app/studyAreaTest/geoms_slc_no_land.geojson")) #no land
  
  samp1 <- st_sample(st_as_sfc(st_bbox(studyArea1)), 1 * mult)
  samp2 <- st_sample(st_as_sfc(st_bbox(studyArea2)), 1 * mult)
  samp3 <- st_sample(st_as_sfc(st_bbox(studyArea3)), 1 * mult)
  Region <- st_read(here::here("app/studyAreaTest/geoms_slc_MarBioRegion.geojson"))
  samp_region <- st_sample(st_as_sfc(st_bbox(Region)), 7 * mult)
  out_samp <- c(samp1, samp2, samp3, samp_region)
  return(out_samp)
}



##### - get random polys ##################################
# This function generates a collection of random polygons inside the region.
# It first generates random points, and then return their associated voronoi 
# polygons.
#
# Inputs:
# 1. mult: how many points should be generated
# 2. out_num: how many polygons should be returned
# Adjusting the inputs will effect the overall size of the polygons
#
# Outputs:
# 1. out_polys: vector of polys geometries with length = out_num 



get_random_polys <- function(mult = 10, out_num = 0.1 * mult) {
  out_samp <- get_random_points(mult)
  out_polys <- st_geometry(out_samp) %>% # make them a geometry
    st_union() %>%  # join all the points
    st_voronoi() %>% # do a voronoi tesselation
    st_collection_extract(type = "POLYGON") %>% 
    sample(out_num)# split up the result into a poly sfc
  return(out_polys)
}



##### - get test point sf ##################################
# This function generates a version of the input sf with a random geometry column
#
# Inputs:
# 1. real_sf: sf object to be reduced/turned into test data
# 2. mult: how many points should be in the final test object
#
# Outputs:
# 1. small_sf: reduced sf filled with test data

gen_test_point_sf <- function(real_sf, mult = 10) {
  samp_size <- mult * 10
  small_sf <- real_sf %>% sample_n(samp_size)
  fake_geo <- get_random_points(mult)  
  small_sf$geometry <- fake_geo
  return(small_sf)
}


##### - get test poly sf ##################################
# This function generates a version of the input sf with a random geometry column
#
# Inputs:
# 1. real_sf: sf object to be reduced/turned into test data
# 2. mult: how many polygons should be in the final test object
#
# Outputs:
# 1. small_sf: reduced sf filled with test data

gen_test_poly_sf <- function(real_sf, n_poly=10) {
  small_sf <- real_sf %>% sample_n(n_poly)
  fake_geo <- get_random_polys(n_poly, n_poly)
  if (is.null(small_sf$geometry)) {
    # need this check for the NBNW_IMPHab_SF
    small_sf$Shape <- fake_geo    
  }
  else {
    small_sf$geometry <- fake_geo
  }
  
  return(small_sf)
}


##### - gen_test_sf ##################################
# This function generates a version of the input sf with a random geometry column
# The length and type of geometry in the final column are dependent on the initial sf
#
# Inputs:
# 1. real_sf: sf object to be reduced/turned into test data
#
# Outputs:
# 1. test_sf: reduced sf filled with test data



gen_test_sf <- function(real_sf) {
  # Geometry type
  point_factor <- sf::st_geometry_type(sf::st_point())
  # set number of samples based on length of sf
  num_samples <- 500
  nrow_real_sf <- nrow(real_sf)
  if (nrow_real_sf < 1000 && nrow_real_sf > 100) {
    num_samples <- 100
  }
  if (nrow_real_sf < 100) {
    num_samples <- nrow_real_sf
  }

  # get test_sf of either points or polys based on type of real_sf
  test_sf <- real_sf
  if (sf::st_geometry_type(real_sf, FALSE) == point_factor) {
    test_sf <- gen_test_point_sf(real_sf, num_samples / 10)
  }
  else {
    test_sf <- gen_test_poly_sf(real_sf, num_samples)
  }
  st_agr(test_sf) == "constant"
  return(test_sf)
}




##### - gen all test data ##################################
# This function generates a test version of all of the sf loaded in in the standard 
# data files.  
#
# Outputs:
#  None.  Data is saved to a file in the data directory.


gen_all_test_data <- function() {
  load(here::here("app/data/OpenData.RData"))
  load(here::here("app/data/OpenData_sardist.RData"))
  load(here::here("app/data/SecureData.RData"))

  sf_list <- list(NBNW_ImpHab_sf, bioregion_sf, BlueWhale_ImpHab_sf, bounds_sf, ClippedCritHab_sf, EBSA_sf,
                  fin_whale_sf, harbour_porpoise_sf, humpback_whale_sf, land10m_sf, land50k_sf, 
                  listed_cetacean_species, listed_fish_invert_species, listed_other_species, 
                  listed_species, obis_cet_sf, obis_fish_sf, rockweed_sf, 
                  RVCatch_sf, RVGSSPECIES, sei_whale_sf, sardist_sf, isdb_sf, ISSPECIESCODES, 
                  leatherback_sf, Legend, marfis_sf, MARFISSPECIESCODES, narwc_sf, whitehead_sf,
                  wsdb_sf)
  sf_names <- c("NBNW_ImpHab_sf", "bioregion_sf", "BlueWhale_ImpHab_sf", "bounds_sf", "ClippedCritHab_sf", "EBSA_sf",
                "fin_whale_sf", "harbour_porpoise_sf", "humpback_whale_sf", "land10m_sf", "land50k_sf", 
                "listed_cetacean_species", "listed_fish_invert_species", "listed_other_species", 
                "listed_species", "obis_cet_sf", "obis_fish_sf", "rockweed_sf", 
                "RVCatch_sf", "RVGSSPECIES", "sei_whale_sf", "sardist_sf", "isdb_sf", "ISSPECIESCODES", 
                "leatherback_sf", "Legend", "marfis_sf", "MARFISSPECIESCODES", "narwc_sf", "whitehead_sf",
                "wsdb_sf")
  
  exclude_list <- c("bounds_sf", "land10m_sf", "land50k_sf")
  
  for (i in 1:length(sf_list)) {
    if (!(sf_names[i] %in% exclude_list) && ("sf" %in% class(sf_list[[i]]))) {
      assign(sf_names[i], gen_test_sf(sf_list[[i]])) 
    }
  }
  
  out_file <- here::here("app/data/testData.Rdata") 
  save(list=sf_names, file=out_file)
}
