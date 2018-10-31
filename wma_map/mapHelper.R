#####################################
#these allow moving AK, HI
proj.string <- "+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs"

to_sp <- function(...){
  map <- maps::map(..., fill=TRUE, plot = FALSE)
  IDs <- sapply(strsplit(map$names, ":"), function(x) x[1])
  map.sp <- map2SpatialPolygons(map, IDs=IDs, proj4string=CRS("+proj=longlat +datum=WGS84"))
  map.sp.t <- spTransform(map.sp, CRS(proj.string))
  return(map.sp.t)
}

shift_sp <- function(sp, scale, shift, rotate = 0, ref=sp, proj.string=NULL, row.names=NULL){
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
###################################

#other shapefile manipulation for regions, wscs, etc
combineStatesWithUS <- function(states, spatialPoly, wsc_name) {
  asString <- paste(states, collapse = "|")
  toUnion <- spatialPoly[grepl(pattern = asString, x = names(spatialPoly))]
  #need to buffer
  toUnion <- gBuffer(toUnion, byid=TRUE, width=0)
  unionedWSC <- gUnaryUnion(toUnion)
  unionedWSC <- spChFIDs(unionedWSC, wsc_name)
  withoutUnion <- spatialPoly[!grepl(pattern = asString, x = names(spatialPoly))]
  appended <- rbind(withoutUnion, unionedWSC)
  return(appended)
}

combineStatesWSCOnly <- function(states, allUSPoly, appendSpatialPoly, wsc_name) {
  asString <- paste(states, collapse = "|")
  toUnion <- allUSPoly[grepl(pattern = asString, x = names(allUSPoly))]
  #need to buffer
  toUnion <- gBuffer(toUnion, byid=TRUE, width=0)
  unionedWSC <- gUnaryUnion(toUnion)
  unionedWSC <- spChFIDs(unionedWSC, wsc_name)
  #withoutUnion <- spatialPoly[!grepl(pattern = asString, x = names(spatialPoly))]
  if(class(appendSpatialPoly) != "SpatialPolygons"){
    return(unionedWSC)
  } else{
    appended <- rbind(appendSpatialPoly, unionedWSC)
    return(appended)
  }
}

