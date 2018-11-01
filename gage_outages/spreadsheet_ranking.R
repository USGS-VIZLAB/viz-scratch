library(tidyverse)
library(googlesheets)
library(dataRetrieval)
library(data.table)
library(stringr)
library(maptools)
library(maps)
library(sp)
library(rgeos)
library(readxl)
library(sf)
token <- gs_auth(cache = FALSE)
title_2 <- gs_title("GOES/DA ISSUE STARTING 2018-10-20")
current_site_list <- gs_read(title_2, ws = "Gages", range = "A5:AA1000")
current_site_list$siteID_15 <- stringr::str_match( current_site_list[[1]], "\\d{15}")[,1]
current_site_list$siteID_10 <- stringr::str_match( current_site_list[[1]], "\\d{10}")[,1]
current_site_list$siteID_9 <- stringr::str_match( current_site_list[[1]], "\\d{9}")[,1]
current_site_list$siteID_8 <- stringr::str_match( current_site_list[[1]], "\\d{8}")[,1]

current_site_list$siteID <- current_site_list$siteID_15
current_site_list$siteID[is.na(current_site_list$siteID)] <- current_site_list$siteID_10[is.na(current_site_list$siteID)]
current_site_list$siteID[is.na(current_site_list$siteID)] <- current_site_list$siteID_9[is.na(current_site_list$siteID)]
current_site_list$siteID[is.na(current_site_list$siteID)] <- current_site_list$siteID_8[is.na(current_site_list$siteID)]
current_site_list$`Replacement DCP implemented in field (Y/N)`[is.na(current_site_list$`Replacement DCP implemented in field (Y/N)`)] <- "N"

current_site_list_use <- filter(current_site_list, `Replacement DCP implemented in field (Y/N)` != "Y") %>% select(-contains("siteID_"))

current_site_list <- current_site_list %>%
  select(siteID) %>%
  filter(!is.na(.)) %>%
  distinct()

siteInfo_orig <- dataRetrieval::readNWISsite(current_site_list$siteID)

siteInfo <- current_site_list %>%
  select(site_no = siteID) %>% 
  left_join(siteInfo_orig, by = "site_no") %>% 
  filter(!(duplicated(site_no))) %>%
  select(site_no, dec_lat_va, dec_long_va, state_cd, site_tp_cd) %>%
  filter(!is.na(dec_lat_va))

siteInfo$state <- NA
siteInfo$state[!is.na(siteInfo$state_cd)]  = dataRetrieval::stateCdLookup(siteInfo$state_cd[!is.na(siteInfo$state_cd)], "postal")

# Ignore  Guam:
siteInfo <- filter(siteInfo, state != "GU")

# sbtools::authenticate_sb()
vizlab::authRemote('sciencebase')
latest_m_flows <- "max_flows_2018-11-01T00Z.rds"
sbtools::item_file_download("5bcf61cde4b0b3fc5cde1742", overwrite_file = TRUE,
                            names = latest_m_flows, destinations = latest_m_flows)
site_nwm_max_flows <- readRDS(latest_m_flows)

siteInfo <- siteInfo %>%
  left_join(select(site_nwm_max_flows, site_no=site, max_flow, `75%`, `95%`, `99%`,`100%`), by="site_no") %>%
  mutate(max_flow = as.numeric(max_flow),
         is_above_75 = max_flow > `75%`,
         is_above_95 = max_flow > `95%`,
         is_above_99 = max_flow >= `99%`,
         is_above_100 = max_flow > `100%`)

siteInfo$above <- "<75"
siteInfo$above[is.na(siteInfo$max_flow)] <- "Unknown"
siteInfo$above[siteInfo$is_above_99] <- ">=99"
siteInfo$above[siteInfo$above == "<75" & siteInfo$is_above_95] <- "95-98"
siteInfo$above[siteInfo$above == "<75" & siteInfo$is_above_75] <- "75-95"

siteInfo$above <- factor(siteInfo$above, levels = c("<75", "75-95", "95-98", ">=99", "Unknown"))

joined <- left_join(current_site_list_use, siteInfo, by = c(siteID = "site_no"))

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

with_nwm_score <- with_nwm_score[!is.na(with_nwm_score$dec_lat_va),]
coords = cbind(with_nwm_score$dec_long_va, with_nwm_score$dec_lat_va)

sites = SpatialPoints(coords, proj4string = CRS(wgs84)) %>%
  spTransform(CRS(proj4string(states.out)))

sites.df <- as.data.frame(sites)

for(i in names(move_variables)){
  shifted <- do.call(shift_sp, c(sp = stuff_to_move[[i]],
                                 move_variables[[i]],
                                 proj.string = proj4string(conus),
                                 row.names = i))
  states.out <- rbind(shifted, states.out, makeUniqueIDs = TRUE)
  
  shifted.sites <- do.call(shift_sp, c(sp = sites[with_nwm_score$state == i,],
                                       move_variables[[i]],
                                       proj.string = proj4string(conus),
                                       ref=stuff_to_move[[i]])) %>%
    as.data.frame %>%
    coordinates()
  
  sites.df[with_nwm_score$state == i, ] <- shifted.sites
  
}

sites.df$`TOTAL METRIC SCORE` <- with_nwm_score$`TOTAL METRIC SCORE`

######################
gsMap <- ggplot() +
  geom_polygon(aes(x = long, y = lat, group = group),
               data = states.out, fill = "grey90",
               alpha = 0.9, color = "grey") +
  coord_sf(datum=NA) +
  geom_point(data = sites.df, size = 2.2, 
             color = "black", pch = 21,#alpha = 0.8,
             aes(x = coords.x1, y=coords.x2, 
                 fill = `TOTAL METRIC SCORE`)) + 
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        plot.title = element_text(hjust = 0.5, vjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        text = element_text()) +
  scale_fill_gradientn(name = "Total Metric Score", colors = rev(c(heat.colors(n=5), "#FFFFFFFF"))) +
  ggtitle(label = paste("Site Outage Summary", Sys.time()), subtitle = paste(nrow(with_nwm_score), "sites currently impacted")) 
gsMap
ggsave(plot = gsMap, filename = "metric_score.png", width = 11, height = 7)
ggsave(plot = gsMap, filename = "metric_score.pdf", width = 11, height = 7)
  