library(googlesheets)
library(dplyr)
library(maptools)
library(ggplot2)
vizlab::authRemote('sciencebase')

token <- gs_auth(cache = FALSE)
title_2 <- gs_title("GOES/DA ISSUE STARTING 2018-10-20")
current_site_list <- gs_read(title_2, ws = "Gages", range = "A5:Q1000")

current_site_list$siteID_15 <- stringr::str_match( current_site_list[[1]], "\\d{15}")[,1]
current_site_list$siteID_10 <- stringr::str_match( current_site_list[[1]], "\\d{10}")[,1]
current_site_list$siteID_9 <- stringr::str_match( current_site_list[[1]], "\\d{9}")[,1]
current_site_list$siteID_8 <- stringr::str_match( current_site_list[[1]], "\\d{8}")[,1]

current_site_list$siteID <- current_site_list$siteID_15
current_site_list$siteID[is.na(current_site_list$siteID)] <- current_site_list$siteID_10[is.na(current_site_list$siteID)]
current_site_list$siteID[is.na(current_site_list$siteID)] <- current_site_list$siteID_9[is.na(current_site_list$siteID)]
current_site_list$siteID[is.na(current_site_list$siteID)] <- current_site_list$siteID_8[is.na(current_site_list$siteID)]

current_site_list$`Replacement DCP implemented in field (Y/N)`[is.na(current_site_list$`Replacement DCP implemented in field (Y/N)`)] <- "N"
current_site_list <- filter(current_site_list, `Replacement DCP implemented in field (Y/N)` != "Y")

current_site_list <- current_site_list %>%
  select(site_no = siteID, Priority = `NWS priority`) %>%
  filter(!is.na(site_no)) %>%
  distinct()

siteInfo_orig <- dataRetrieval::readNWISsite(current_site_list$site_no)

sites_with_NWIS <- current_site_list %>%
  left_join(siteInfo_orig, by = "site_no") 

sites_with_NWIS <- sites_with_NWIS %>%
  select(site_no, Priority, station_nm, dec_lat_va, dec_long_va, state_cd, site_tp_cd) %>%
  filter(!is.na(dec_lat_va))

Surface_Water <- c("ES","LK","OC","OC-CO","ST","ST-CA","ST-DCH","ST-TS","WE")
Groundwater <- c("GW","GW-CR", "GW-EX","GW-HZ","GW-IW","GW-MW","GW-TH",
                 "SB","SB-CV","SB-GWD","SB-TSM","SB-UZ")
Spring <- c("SP")
Atmospheric <- "AT"
sites_with_NWIS$state <- NA
sites_with_NWIS$state[!is.na(sites_with_NWIS$state_cd)]  = dataRetrieval::stateCdLookup(sites_with_NWIS$state_cd[!is.na(sites_with_NWIS$state_cd)], "postal")

siteInfo <- sites_with_NWIS
# Ignore the 1 in Guam:
siteInfo <- filter(siteInfo, state != "GU")

siteInfo$type <- "Other"
siteInfo$type[siteInfo$site_tp_cd %in% Surface_Water] <- "Surface Water"
siteInfo$type[siteInfo$site_tp_cd %in% Groundwater] <- "Groundwater"
siteInfo$type[siteInfo$site_tp_cd %in% Spring] <- "Spring"
siteInfo$type[siteInfo$site_tp_cd %in% Atmospheric] <- "Atmospheric"

siteInfo$type <- factor(siteInfo$type, levels = c("Surface Water","Groundwater","Spring","Atmospheric","Other"))

levels(siteInfo$type)[levels(siteInfo$type)=="Surface Water"] <- paste0("Surface Water (",sum(siteInfo$site_tp_cd %in% Surface_Water, na.rm = TRUE),")")
levels(siteInfo$type)[levels(siteInfo$type)=="Groundwater"] <- paste0("Groundwater (",sum(siteInfo$site_tp_cd %in% Groundwater, na.rm = TRUE),")")
levels(siteInfo$type)[levels(siteInfo$type)=="Spring"] <- paste0("Spring (",sum(siteInfo$site_tp_cd %in% Spring, na.rm = TRUE),")")
levels(siteInfo$type)[levels(siteInfo$type)=="Atmospheric"] <- paste0("Atmospheric (",sum(siteInfo$site_tp_cd %in% Atmospheric, na.rm = TRUE),")")
levels(siteInfo$type)[levels(siteInfo$type)=="Other"] <- paste0("Other (",sum(!(siteInfo$site_tp_cd %in% c(Surface_Water, Groundwater, Spring, Atmospheric)), na.rm = TRUE),")")

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
coords = cbind(siteInfo$dec_long_va, siteInfo$dec_lat_va)

sites = SpatialPoints(coords, proj4string = CRS(wgs84)) %>%
  spTransform(CRS(proj4string(states.out)))

sites.df <- as.data.frame(sites)

for(i in names(move_variables)){
  shifted <- do.call(shift_sp, c(sp = stuff_to_move[[i]],
                                 move_variables[[i]],
                                 proj.string = proj4string(conus),
                                 row.names = i))
  states.out <- rbind(shifted, states.out, makeUniqueIDs = TRUE)
  
  shifted.sites <- do.call(shift_sp, c(sp = sites[siteInfo$state == i,],
                                       move_variables[[i]],
                                       proj.string = proj4string(conus),
                                       ref=stuff_to_move[[i]])) %>%
    as.data.frame %>%
    coordinates()
  
  sites.df[siteInfo$state == i, ] <- shifted.sites
  
}

siteInfo$Priority[is.na(siteInfo$Priority) | siteInfo$Priority == 0] <- "Other"
sites.df$Priority <- as.factor(siteInfo$Priority)

levels(sites.df$Priority)[levels(sites.df$Priority) == "1"] <- paste0("1 (",sum(sites.df$Priority == "1", na.rm = TRUE),")")
levels(sites.df$Priority)[levels(sites.df$Priority) == "2"] <- paste0("2 (",sum(sites.df$Priority == "2", na.rm = TRUE),")")
levels(sites.df$Priority)[levels(sites.df$Priority) == "3"] <- paste0("3 (",sum(sites.df$Priority == "3", na.rm = TRUE),")")
levels(sites.df$Priority)[levels(sites.df$Priority) == "4"] <- paste0("4 (",sum(sites.df$Priority == "4", na.rm = TRUE),")")
levels(sites.df$Priority)[levels(sites.df$Priority) == "5"] <- paste0("5 (",sum(sites.df$Priority == "5", na.rm = TRUE),")")
levels(sites.df$Priority)[levels(sites.df$Priority) == "Other"] <- paste0("Other (",sum(sites.df$Priority == "Other", na.rm = TRUE),")")

#note: assuming there are no priority 1 sites, add red back if there are
set_colors <- c("orange","yellow","steelblue","darkolivegreen3","grey")
names(set_colors) <- levels(sites.df$Priority)

################################
# Clip QPF to CONUS
################################
sbtools::item_file_download("5bcf61cde4b0b3fc5cde1742", names = "qpf.rds", destinations = "qpf.rds", overwrite_file = TRUE)
qpf <- readRDS("qpf.rds")
qpf <- sf::st_intersection(qpf, sf::st_buffer(sf::st_as_sf(conus), 0))

################################
# Plot it up
################################

gsMap <- ggplot() +
  geom_polygon(aes(x = long, y = lat, group = group),
               data = states.out, fill = "grey90",
               alpha = 0.9, color = "grey") +
  geom_sf(aes(fill = QPF), data = qpf, alpha = 0.5, color = "transparent") +
  scale_fill_gradient("7-day QPF [in]", low = "#f2f2f2", high = "#4186f4") +
  coord_sf(datum=NA) +
  geom_polygon(aes(x = long, y = lat, group = group),
               data = states.out, fill = NA,
               alpha = 0.9, color = "grey") +
  geom_point(data = sites.df, size = 2, aes(x = coords.x1, y=coords.x2, color = Priority)) + 
  scale_color_manual(values = set_colors, breaks = levels(sites.df$Priority)) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 1)) +
  ggtitle(label = paste("Site Outage Summary", Sys.time()), subtitle = paste(nrow(siteInfo), "sites currently impacted")) +
  guides(color = guide_legend(title="Priority", order = 1)) + 
  labs(caption = "Quantitative Precipitation Forecast (QPF) VALID: 12Z 2018-11-01 THRU 12Z 2018-11-08\n")

gsMap
ggsave(gsMap, filename = "site_outages_priority.pdf", width = 11, height = 7)
ggsave(gsMap, filename = "site_outages_priority.png", width = 11, height = 7)
