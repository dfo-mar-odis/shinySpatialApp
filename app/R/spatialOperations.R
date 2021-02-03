valid_bbox <- function(xmin, xmax, ymin, ymax, crs_in = 4326) {
  out <- st_as_sf(
    st_as_sfc(
      st_bbox(
        c(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), 
        crs = crs_in
      )
    )
  )
  if (crs_in != 4326) {
    st_transform(out, 4326)
  } else out
}

valid_points <- function(x, y, crs_in = 4326) {
  out <- st_as_sf(
    data.frame(x = x , y = y), 
    coords = c("x", "y"), 
    crs = crs_in
  )  
  if (crs_in != 4326) {
    st_transform(out, 4326)
  } else out
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