library(maps)
library(maptools)
library(sp)
library(ggplot2)
library(dplyr)
library(rgeos)
rm(list = ls())
source('mapHelper.R')
mapData <- read.csv('wma_locations.csv', stringsAsFactors = FALSE)

#need to set state for carribbean WSC
mapData$STATE[mapData$CITY == "GUAYNABO"] <- "PR"

conus <- to_sp('state')

# thanks to Bob Rudis (hrbrmstr):
# https://github.com/hrbrmstr/rd3albers

# -- if moving any more states, do it here: --
move_variables <- list(
  AK = list(scale=0.4, shift = c(130,-490), rotate=-50),
  HI = list(scale=1, shift=c(365, -50), rotate=-35),
  PR = list(scale=2.5, shift = c(-250, 10), rotate=20)
)

stuff_to_move <- list(
  AK = to_sp("world", "USA:alaska"),
  HI = to_sp("world", "USA:hawaii"),
  PR = to_sp("world", "Puerto Rico")
)

states.out <- conus

wgs84 <- "+init=epsg:4326"
coords = cbind(mapData$LONGITUDE, mapData$LATITUDE)
 sites = SpatialPoints(coords, proj4string = CRS(wgs84)) %>% 
   spTransform(CRS(proj4string(states.out)))

 sites.df <- as.data.frame(sites)
 
for(i in names(move_variables)){
  shifted <- do.call(shift_sp, c(sp = stuff_to_move[[i]], 
                                 move_variables[[i]],  
                                 proj.string = proj4string(conus),
                                 row.names = i))
  states.out <- rbind(shifted, states.out, makeUniqueIDs = TRUE)
  
  shifted.sites <- do.call(shift_sp, c(sp = sites[mapData$STATE == i,],
                                       move_variables[[i]],
                                       proj.string = proj4string(conus),
                                       ref=stuff_to_move[[i]])) %>%
    as.data.frame %>%
    coordinates()

  sites.df[mapData$STATE == i, ] <- shifted.sites
  
}

#create regions
regions <- list( NE = states.out[c('maine', 'new hampshire', 'vermont', 
                                   'massachusetts', 'rhode island', 'connecticut',
                                   'new york', 'delaware', 'new jersey', 'maryland',
                                   'pennsylvania', 'west virginia', 'virginia', 
                                   'district of columbia')],
                 SE = states.out[c('north carolina', 'south carolina', 'georgia', 
                                   'florida', 'mississippi', 'alabama', 'tennessee',
                                   'arkansas', 'louisiana', 'PR')],
                 SW = states.out[c('arizona', 'utah', 'colorado', 'new mexico', 
                                   'texas', 'oklahoma', 'kansas')],
                 MW = states.out[c('north dakota', 'south dakota', 'nebraska',
                                   'minnesota', 'wisconsin', 'iowa', 'missouri',
                                   'illinois', 'indiana', 'ohio', 'michigan', 
                                   'kentucky')],
                 PAC = states.out[c("california", "nevada", "HI")],
                 NW = states.out[c("washington", "oregon", "idaho",
                                   "montana", "wyoming")],
                 AK = states.out[c("AK")])

colors <- c("#E69F0080", "#56B4E980", "#009E7380", "#F0E44280", "#0072B280", "#D55E0080", "#CC79A780")
regionNames <- c("NE", "SE", "SW", "MW", "PAC", "NW", "AK")
color_region_df <- data.frame(regionNames, colors, stringsAsFactors = FALSE)
# gsMap <- ggplot() +
#   # geom_polygon(aes(x = long, y = lat, group = group),
#   #              data = tri, fill = "blue",
#   #              alpha = 0.5, color = "white") +
#   theme_minimal() +
#   theme(panel.grid = element_blank(),
#         axis.text = element_blank(),
#         axis.title = element_blank()) 
#dev.off()
#png('wscMap.png', units = "in", width = 11, height = 8.5, res = 300)

pdf('wscMap.pdf', width = 11, height = 8.5)
plot(states.out, border = "white") #get blank map set up
for(i in 1:nrow(color_region_df)) {
  # gsMap <- gsMap + geom_polygon(aes(x = long, y = lat, group = group),
  #                                     data = regions[[color_region_df$regionName[i]]], 
  #                       fill = color_region_df$color[i],
  #                                     alpha = 0.5, color = "white")
  if(i == 1){
    plot(regions[[color_region_df$regionName[i]]], col = color_region_df$color[i],
         border = "white", add = TRUE)
  } else{
    plot(regions[[color_region_df$regionName[i]]], col = color_region_df$color[i],
         border = "white", add = TRUE)
  }
}

#create merged WSC polygons from states out
#going to remove states to merge, union them, then add them back as single shapes
#wscPolyies <- states.out
combinedWSC <- list( UPGL = c("wisconsin", "minnesota", "michigan"),
                     WVVA = c("west virginia", "virginia"),
                     DAK = c("north dakota", "south dakota"),
                     INKYOH = c("indiana", 'kentucky', 'ohio'),
                     MTWY = c("montana", "wyoming"),
                     LMG = c("louisiana", "arkansas", "mississippi", "alabama",
                             "tennessee"),
                     SA = c("north carolina", "south carolina", "georgia"),
                     FLCA = c("PR", "florida"),
                     NE = c("maine", "rhode island", "connecticut",
                            "vermont", "new hampshire", "massachusetts"),
                     ILIA = c("illinois", "iowa"),
                     MDDEDC = c("district of columbia", "delaware", "maryland"))




#wscPolyies <- NA
wscPolyies <- states.out
for(i in 1:length(combinedWSC)) {
  wsc <- combinedWSC[[i]]
  wsc_name <- names(combinedWSC)[i]
  #wscPolyies <- combineStatesWSCOnly(states = wsc, appendSpatialPoly = wscPolyies,
  #                                   allUSPoly = states.out, wsc_name)
  wscPolyies <- combineStatesWithUS(states = wsc, spatialPoly = wscPolyies, wsc_name = wsc_name)
}

UPGL_polygon <- wscPolyies['UPGL']
wscPolyies <- wscPolyies[!grepl(pattern = "UPGL", x = names(wscPolyies))]
plot(wscPolyies, add = TRUE, border = "darkgray", lwd = 3)

#kinda hacky solution to eliminate Lake MI shoreline
UPGL_lines <- as(UPGL_polygon, "SpatialLines")
UPGL_lines@lines[[1]]@Lines[[1]]@coords <- UPGL_lines@lines[[1]]@Lines[[1]]@coords[c(178:370, 1:16),]
UPGL_lines@lines[[1]]@Lines[[2]]@coords <- UPGL_lines@lines[[1]]@Lines[[2]]@coords[1:660,]
plot(UPGL_lines, add = TRUE, col = "darkgray", lwd = 3)

# wscPolyies <- gBuffer(wscPolyies, byid = TRUE, width = 0)
# borders = gDifference(
#       as(wscPolyies,"SpatialLines"),
#       as(gUnaryUnion(wscPolyies),"SpatialLines"),
#       byid=TRUE)

regionHQs <- sites.df[mapData$REGIONAL_HQ,]
wsc_pts <- sites.df[!is.na(mapData$WSC_director),]
#points(regionHQs, pch = 23, cex = 1.5, col = "yellow")

coordinates(regionHQs) <-  ~coords.x1+coords.x2
coordinates(wsc_pts) <-  ~coords.x1+coords.x2
plot(regionHQs, pch = 23, cex = 2, bg = "yellow", add = TRUE)
plot(wsc_pts, pch = 21, bg="blue", add = TRUE, cex = 1.3 )
legend(x = grconvertX(0.7, from = "npc"), grconvertY(0.99, from = 'npc'), 
       legend = c("Regional HQ", "WSC Director", "WSC boundary", "Field office"),
       pch = c(23, 21, NA, 15), pt.bg = c("yellow", "blue", NA, NA),
       lwd = c(1, 1, 3), col = c("black", "black", "darkgray", "darkgreen"), 
       lty = c(NA, NA, 1), pt.cex = c(2,1, 1, 0.6))

#boxes around moved states
#alaska
rect(xleft = grconvertX(0.15, from = "npc"), 
     ybottom = grconvertY(0.05, from = "npc"), 
     xright = grconvertX(0.39, from = "npc"), 
     ytop = grconvertY(0.27, from = "npc"))

#hawaii
rect(xleft = grconvertX(0.02, from = "npc"), 
     ybottom = grconvertY(0.29, from = "npc"), 
     xright = grconvertX(0.16, from = "npc"), 
     ytop = grconvertY(0.425, from = "npc"))

#puerto rico
rect(xleft = grconvertX(0.6, from = "npc"), 
     ybottom = grconvertY(0.06, from = "npc"), 
     xright = grconvertX(0.76, from = "npc"), 
     ytop = grconvertY(0.16, from = "npc"))

fieldOff_logical <- !mapData$REGIONAL_HQ & is.na(mapData$WSC_director)
fieldOff <- sites.df[fieldOff_logical,]
coordinates(fieldOff) <-  ~coords.x1+coords.x2
plot(fieldOff, add = TRUE, pch = 15, col = "darkgreen", cex = 0.6)
box()
title(main = "USGS Water Locations, 2017")
dev.off()
