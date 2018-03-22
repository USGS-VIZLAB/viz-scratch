get_dots <- function(json_file, proj.string, state_name){
  
  data <- read_json(json_file)$objects$foo$geometries
  
  pt_coords <- matrix(data = rep(NA, length(data)*2), ncol = 2)
  for (j in seq_len(length(data))){
    
    coord <- data[[j]]$coordinates
    state_abb <- data[[j]]$properties$STATE_ABBV
    if (state_abb == state_name){
      pt_coords[j, ] <- c(coord[[1]][1], coord[[2]][1])
    }
    
  }
  
  
  points <- pt_coords[!is.na(pt_coords[, 1]), ] %>% 
      sp::SpatialPoints(proj4string = CRS("+proj=longlat +datum=WGS84")) %>% 
      sp::spTransform(CRS(proj.string))
  
  # points <- cbind(locations$dec_long_va, locations$dec_lat_va) %>% 
  #   sp::SpatialPoints(proj4string = CRS("+proj=longlat +datum=WGS84")) %>% 
  #   sp::spTransform(CRS(proj.string)) %>% 
  #   sp::SpatialPointsDataFrame(data = locations[c('site_no')])
  # 
  return(points)
}