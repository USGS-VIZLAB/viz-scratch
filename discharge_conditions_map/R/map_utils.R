# mostly from gages-through-ages project

#' take map arguments and return a projected sp object
#' 
#' @param \dots arguments passed to \code{\link[maps]{map}} excluding \code{fill} and \code{plot}
#' 
proj.string <- "+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs"
to_sp <- function(...){
  library(mapdata)
  map <- maps::map(..., fill=TRUE, plot = FALSE)
  IDs <- sapply(strsplit(map$names, ":"), function(x) x[1])
  map.sp <- map2SpatialPolygons(map, IDs=IDs, proj4string=CRS("+proj=longlat +datum=WGS84"))
  map.sp.t <- spTransform(map.sp, CRS(proj.string))
  return(map.sp.t)
}

#' @param locations a data.frame with dec_long_va and dec_lat_va
points_sp <- function(locations){
  library(dplyr)
  
  points <- cbind(locations$dec_long_va, locations$dec_lat_va) %>% 
    sp::SpatialPoints(proj4string = CRS("+proj=longlat +datum=WGS84")) %>% 
    sp::spTransform(CRS(proj.string)) %>% 
    sp::SpatialPointsDataFrame(data = locations[c('site_no')])
}

get_shifts <- function(){
  list(AK = list(scale = 0.47, shift = c(90,-465), rotate = -50),
       HI = list(scale = 1.5, shift = c(520, -110), rotate = -35),
       PR = list(scale = 3.5, shift = c(-130, 90), rotate=20))
}

get_moves <- function(){
  list(
    AK = to_sp("world", "USA:alaska"),
    HI = to_sp("world", "USA:hawaii"),
    PR = to_sp("world2Hires", "Puerto Rico")
  )
}

#' create the sp object 
#'
state_sp <- function(){
  
  shifts <- get_shifts()
  
  stuff_to_move <- get_moves()
  

  states.out <- to_sp('state')
  for(i in names(shifts)){
    shifted <- do.call(shift_sp, c(sp = stuff_to_move[[i]], 
                                   shifts[[i]],  
                                   proj.string = proj4string(states.out),
                                   row.names = i))
    states.out <- rbind(shifted, states.out, makeUniqueIDs = TRUE)
  }
  
  return(states.out)
}

sites_sp <- function(site_nos){
  sites <- readNWISsite(site_nos) %>% filter(!is.na(dec_lat_va))
  code.map <- c(AK = "02", HI = "15", PR = "72")
  
  shifts <- get_shifts()
  
  stuff_to_move <- get_moves()
  
  sites.out <- sites %>% filter(!state_cd %in% code.map) %>% 
    points_sp()
  
  for (region in names(code.map)){
    sites.tmp <- sites %>% filter(state_cd %in% code.map[[region]]) %>% 
      points_sp()
    sites.tmp <- do.call(shift_sp, c(sp = sites.tmp, ref = stuff_to_move[[region]], 
                                     shifts[[region]]))
    sites.out <- rbind(sites.out, sites.tmp)
  }
  return(sites.out)
}


shift_sp <- function(sp, scale = NULL, shift = NULL, rotate = 0, ref=sp, proj.string=NULL, row.names=NULL){
  if (is.null(scale) & is.null(shift) & rotate == 0){
    return(obj)
  }
  orig.cent <- rgeos::gCentroid(ref, byid=TRUE)@coords
  scale <- max(apply(bbox(ref), 1, diff)) * scale
  obj <- elide(sp, rotate=rotate, center=orig.cent, bb = bbox(ref))
  ref <- elide(ref, rotate=rotate, center=orig.cent, bb = bbox(ref))
  obj <- elide(obj, scale=scale, center=orig.cent, bb = bbox(ref))
  ref <- elide(ref, scale=scale, center=orig.cent, bb = bbox(ref))
  new.cent <- rgeos::gCentroid(ref, byid=TRUE)@coords
  obj <- elide(obj, shift=shift*10000+c(orig.cent-new.cent))
  if (is.null(proj.string)){
    proj4string(obj) <- proj4string(sp)
  } else {
    proj4string(obj) <- proj.string
  }
  
  if (!is.null(row.names)){
    row.names(obj) <- row.names
  }
  return(obj)
}