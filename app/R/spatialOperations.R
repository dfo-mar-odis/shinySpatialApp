# The functions below ensure that input geoms are properly formatted.

valid_bbox <- function(xmin, xmax, ymin, ymax, nm, crs_in = 4326) {
  out <- st_sf(
    name = nm,
    geometry = st_as_sfc(
      st_bbox(
        c(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), 
      )
    ),
    crs = crs_in
  )
  if (crs_in != 4326) {
    st_transform(out, 4326)
  } else out
}

valid_points <- function(x, y, nm, crs_in = 4326) {
  out <- st_as_sf(
    data.frame(name = nm, x = x, y = y), 
    coords = c("x", "y"), 
    crs = crs_in
  )  
  if (crs_in != 4326) {
    st_transform(out, 4326)
  } else out
}

# no need for CRS because edit map uses 4326 
valid_from_map <- function(x, nm) {
  if (nrow(x) > 1) nm <- paste0(nm, "_", seq_len(nrow(x)))
  x$name <- nm
  x["name"]
}

# no need for CRS because edit map uses 4326 
valid_import <- function(x, nm) {
  out <- st_read(x)
  out["name"] <- nm
  if (!identical(st_crs(out), st_crs(4326))) {
    out <- st_transform(out, crs = 4326)
  }
  out["name"]
}


# use projection 3857 to add buffer
add_buffer <- function(x, buffer) {
  if (buffer > 0) {
    st_transform(
      st_buffer(st_transform(x, crs = 3857), dist = buffer),
      crs = 4326
    )
  } else x 
}

# function to append/stack sf object and add buffer
append_geom <- function(geoms, x, buffer) {
  tmp <- add_buffer(x, buffer)
  if (is.null(geoms)) tmp else rbind(geoms, tmp)
}