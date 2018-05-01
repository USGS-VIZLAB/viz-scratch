get_dots <- function(json_file, data_file, proj.string="+proj=longlat +datum=WGS84", state_name = NULL){
  
  
  centroids <- read_json(json_file)$objects$centroids$geometries
  centroid_meta <- read_tsv(data_file)
  
  NA_out <- rep(NA, length(centroids))
  
  pt_coords <- matrix(data = c(NA_out, NA_out), ncol = 2)
  
  dot_data <- data.frame(total = NA_out, thermoelectric = NA_out, 
                         publicsupply = NA_out, irrigation = NA_out, industrial = NA_out)
  
  
  for (j in seq_len(length(centroids))){
    this_dot <- centroids[[j]]
    coord <- this_dot$coordinates
    state_abb <- this_dot$properties$STATE_ABBV
    
    if (is.null(state_name) || state_abb == state_name){
      pt_coords[j, ] <- c(coord[[1]][1], coord[[2]][1])
    }
    
    this_meta <- filter(centroid_meta, GEOID == this_dot$properties$GEOID)[names(dot_data)]
    dot_data[j, ] <- this_meta
  }
  
  dot_data$state <- sapply(centroids, function(x) x$properties$STATE_ABBV)
  
  points <- pt_coords[!is.na(pt_coords[, 1]), ] %>% 
    sp::SpatialPoints(proj4string = CRS("+proj=longlat +datum=WGS84")) %>% 
    sp::spTransform(CRS(proj.string)) %>% 
    sp::SpatialPointsDataFrame(data = dot_data[!is.na(pt_coords[, 1]), ])
  
  
  return(points)
}

get_state_totals <- function(json_file, state){
  state_totals <- read_json(json_file)
  abrvs <- sapply(state_totals, function(x) x$abrv)
  state_i <- which(abrvs == state)
  
  totals_out <- data.frame(total = NA_character_, thermoelectric = NA_character_, 
                           publicsupply = NA_character_, irrigation = NA_character_, industrial = NA_character_,
                           other = NA_character_)
  totals_numeric <- data.frame(total = NA_integer_, thermoelectric = NA_integer_, 
                               publicsupply = NA_integer_, irrigation = NA_integer_, industrial = NA_integer_)
  this_state <- state_totals[[state_i]]$use
  for (use_i in 1:length(this_state)){
    cat_name <- this_state[[use_i]]$category
    text_num <- this_state[[use_i]]$fancynums
    totals_out[[cat_name]] <- text_num
    totals_numeric[[cat_name]] <- this_state[[use_i]]$wateruse
  }
  other_num <- totals_numeric$total-rowSums(totals_numeric[!names(totals_numeric) %in% "total"])
  totals_out$other <- prettyNum(other_num, big.mark=",",scientific=FALSE)

  return(totals_out)
}

rank_states <- function(dots){
  county_data <- dots@data
  
  state_totals <- county_data %>% group_by(state) %>% 
    summarize(total = sum(total), irr = sum(irrigation), therm = sum(thermoelectric))
  return(state_totals)
}