library(tidyverse)
library(googlesheets)
library(dataRetrieval)
library(data.table)
library(dplyr)
library(stringr)
library(maptools)
library(maps)
library(sp)
library(rgeos)
library(ggplot2)
library(readxl)
library(sf)
library(googlesheets)
sites.df <- readRDS('sites.df_w_site_info.rds')
title_2 <- gs_title("GOES/DA ISSUE STARTING 2018-10-20")
current_site_list <- gs_read(title_2, range = "A5:AA1000")
current_site_list$siteID_15 <- stringr::str_match( current_site_list[[1]], "\\d{15}")[,1]
current_site_list$siteID_10 <- stringr::str_match( current_site_list[[1]], "\\d{10}")[,1]
current_site_list$siteID_9 <- stringr::str_match( current_site_list[[1]], "\\d{9}")[,1]
current_site_list$siteID_8 <- stringr::str_match( current_site_list[[1]], "\\d{8}")[,1]

current_site_list$siteID <- current_site_list$siteID_15
current_site_list$siteID[is.na(current_site_list$siteID)] <- current_site_list$siteID_10[is.na(current_site_list$siteID)]
current_site_list$siteID[is.na(current_site_list$siteID)] <- current_site_list$siteID_9[is.na(current_site_list$siteID)]
current_site_list$siteID[is.na(current_site_list$siteID)] <- current_site_list$siteID_8[is.na(current_site_list$siteID)]
current_site_list$`Replacement DCP implemented in field (Y/N)`[is.na(current_site_list$`Replacement DCP implemented in field (Y/N)`)] <- "N"
current_site_list_use <- filter(current_site_list, `Replacement DCP implemented in field (Y/N)` != "Y") %>% 
  select(-contains("siteID_")) %>% 
  mutate(`AVAILABLE UNIT (Y/N)` = ifelse(is.na(`AVAILABLE UNIT (Y/N)`), 
                yes = "N", no = `AVAILABLE UNIT (Y/N)`)) %>% 
  filter(`AVAILABLE UNIT (Y/N)` != "Y")
  

joined <- left_join(current_site_list_use, sites.df, by = c(siteID = "site_no"))
names(joined)[20:25] <- c("nwm_10day_score", "international", "internal_real_time", "muni_irrigation",
                          "wsc_coop", "high_hazard_asset")
with_nwm_score <- joined %>% mutate(nwm_10day_score = recode(above, `<75` = 0, `75-95` = 5, 
                                                             `95-98` = 10, `>=99` = 20, .default = 0)) %>% 
  mutate(`TOTAL METRIC SCORE` = rowSums(select(., nwm_10day_score:high_hazard_asset), na.rm = TRUE))
write_csv(with_nwm_score, path = "total_metric_computed.csv")

################################
# Setup Map
################################\
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

# thanks to Bob Rudis (hrbrmstr):
# https://github.com/hrbrmstr/rd3albers

# -- if moving any more states, do it here: --
move_variables <- list(
  AK = list(scale=0.33, shift = c(80,-450), rotate=-50),
  HI = list(scale=1, shift=c(520, -110), rotate=-35)
)

stuff_to_move <- list(
  AK = to_sp("world", "USA:alaska"),
  HI = to_sp("world", "USA:hawaii")
)

conus <- to_sp('state')
states.out <- conus
wgs84 <- "+init=epsg:4326"
#coords = cbind(siteInfo$dec_long_va, siteInfo$dec_lat_va)

# sites = SpatialPoints(coords, proj4string = CRS(wgs84)) %>%
#   spTransform(CRS(proj4string(states.out)))
# 
# sites.df <- as.data.frame(sites)

for(i in names(move_variables)){
  shifted <- do.call(shift_sp, c(sp = stuff_to_move[[i]],
                                 move_variables[[i]],
                                 proj.string = proj4string(conus),
                                 row.names = i))
  states.out <- rbind(shifted, states.out, makeUniqueIDs = TRUE)
  
  # shifted.sites <- do.call(shift_sp, c(sp = sites[siteInfo$state == i,],
  #                                      move_variables[[i]],
  #                                      proj.string = proj4string(conus),
  #                                      ref=stuff_to_move[[i]])) %>%
  #   as.data.frame %>%
  #   coordinates()
  
  # sites.df[siteInfo$state == i, ] <- shifted.sites
  
}
######################
gsMap <- ggplot() +
  geom_polygon(aes(x = long, y = lat, group = group),
               data = states.out, fill = "grey70",
               alpha = 0.9, color = "grey25", size = 0.25) +
  #geom_sf(aes(fill = QPF), data = qpf, alpha = 0.5, color = "transparent") +
  #scale_fill_gradient("7-day QPF [in]", low = "#f2f2f2", high = "#4186f4") +
  coord_sf(datum=NA) +
  # geom_polygon(aes(x = long, y = lat, group = group),
  #              data = states.out, fill = NA,
  #              alpha = 0.9, color = "grey") +
  geom_point(data = with_nwm_score, size = 0.5, #alpha = 0.8,
             aes(x = coords.x1, y=coords.x2, color = `TOTAL METRIC SCORE`)) + 
  #color = NWS, shape = type)) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        plot.title = element_text(hjust = 0.5, vjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.justification = "top",
        text = element_text(family = "Abel")) +
  scale_color_gradient(low = "white", high = "red") +
  ggtitle(label = paste("Site Outage Summary", Sys.Date()), subtitle = paste(nrow(with_nwm_score), "sites currently impacted with no replacement unit")) 
ggsave(plot = gsMap, filename = "metric_score.png")
