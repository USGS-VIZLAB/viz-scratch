# Utility functions for shifting

to_sf <- function(..., projection = "+proj=longlat +datum=WGS84"){
  map <- maps::map(..., fill=TRUE, plot = FALSE)
  map.sf <- st_as_sf(map)
  map.sf.t <- st_transform(map.sf, projection)
  return(map.sf.t)
}

shift_sf <- function(obj_sf, rotation_deg, scale, shift, projection, ref_sf = NULL) {
  
  stopifnot(!is.na(st_crs(obj_sf)))
  if(is.null(ref_sf)) ref_sf <- obj_sf
  
  obj_sf_transf <- st_transform(obj_sf, projection)
  obj_sfg <- st_geometry(obj_sf)
  obj_centroid <- st_centroid(obj_sfg)
  
  # Rotate and scale obj_sf
  obj_sf_rot_scaled <- (obj_sfg - obj_centroid) * rot(rotation_deg*pi/180) * scale + obj_centroid
  
  # Borrowing these fixes to scale & shift from sp code 
  #   See https://github.com/USGS-VIZLAB/gages-through-ages/blob/master/scripts/process/process_map.R#L91
  obj_centroid_new <- st_centroid(obj_sf_rot_scaled)
  new_shift <- shift*10000+c(st_coordinates(obj_centroid)-st_coordinates(obj_centroid_new))
  
  # Shift obj_sf
  obj_sf_shifted <- obj_sf_rot_scaled + new_shift
  st_crs(obj_sf_shifted) <- projection
  
  return(obj_sf_shifted)
}

rot <- function(a) matrix(c(cos(a), sin(a), -sin(a), cos(a)), 2, 2)
