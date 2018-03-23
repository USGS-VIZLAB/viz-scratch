get_dots <- function(json_file, proj.string, state_name){
  
  data <- read_json(json_file)$objects$foo$geometries
  
  NA_out <- rep(NA, length(data))
  
  pt_coords <- matrix(data = c(NA_out, NA_out), ncol = 2)
  
  dot_data <- data.frame(total = NA_out, thermoelectric = NA_out, 
                         publicsupply = NA_out, irrigation = NA_out, industrial = NA_out)
  for (j in seq_len(length(data))){
    this_dot <- data[[j]]
    coord <- this_dot$coordinates
    state_abb <- this_dot$properties$STATE_ABBV
    if (state_abb == state_name){
      pt_coords[j, ] <- c(coord[[1]][1], coord[[2]][1])
    }
    dot_data[j, ] <- this_dot$properties[names(dot_data)]
    
  }
  
  
  points <- pt_coords[!is.na(pt_coords[, 1]), ] %>% 
    sp::SpatialPoints(proj4string = CRS("+proj=longlat +datum=WGS84")) %>% 
    sp::spTransform(CRS(proj.string)) %>% 
    sp::SpatialPointsDataFrame(data = dot_data[!is.na(pt_coords[, 1]), ])
  
  
  return(points)
}