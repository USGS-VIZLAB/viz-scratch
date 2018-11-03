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

# sbtools::authenticate_sb()
vizlab::authRemote('sciencebase')

################################
# Get Latest sites
################################
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

Surface_Water <- c("ES","LK","OC","OC-CO","ST","ST-CA","ST-DCH","ST-TS","WE")
Groundwater <- c("GW","GW-CR", "GW-EX","GW-HZ","GW-IW","GW-MW","GW-TH",
                 "SB","SB-CV","SB-GWD","SB-TSM","SB-UZ")
Spring <- c("SP")
Atmospheric <- "AT"
siteInfo$state <- NA
siteInfo$state[!is.na(siteInfo$state_cd)]  = dataRetrieval::stateCdLookup(siteInfo$state_cd[!is.na(siteInfo$state_cd)], "postal")

siteInfo$type <- "Other"
siteInfo$type[siteInfo$site_tp_cd %in% Surface_Water] <- "Surface Water"
siteInfo$type[siteInfo$site_tp_cd %in% Groundwater] <- "Groundwater"
siteInfo$type[siteInfo$site_tp_cd %in% Spring] <- "Spring"
siteInfo$type[siteInfo$site_tp_cd %in% Atmospheric] <- "Atmospheric"

siteInfo$type <- factor(siteInfo$type, levels = c("Surface Water","Groundwater","Spring","Atmospheric","Other"))

# Ignore  Guam:
siteInfo <- filter(siteInfo, state != "GU")
 
# fwrite(sitesInfo, "sites_NWIS_thurs.csv")
# 
# sbtools::item_append_files("5bcf61cde4b0b3fc5cde1742",
#                             files = "sites_NWIS_thurs.csv", destinations = "sites_NWIS_thurs.csv")
# 
# sbtools::item_file_download("5bcf61cde4b0b3fc5cde1742", overwrite_file = TRUE,
#                             names = "sites_NWIS_thurs.csv", destinations = "sites_NWIS.csv")
# siteInfo <- fread("sites_NWIS.csv", colClasses = c(site_no = "character"))

################################
# Get NWM Max Flows
################################

# Used to retrieve NWM flows.
saveRDS(siteInfo, "siteInfo.rds")

latest_m_flows <- "max_flows_2018-11-03T00Z.rds"

sbtools::item_file_download("5bcf61cde4b0b3fc5cde1742", overwrite_file = TRUE,
                            names = latest_m_flows, destinations = latest_m_flows)

site_nwm_max_flows <- readRDS(latest_m_flows)


# max_flow_files <- c("max_flows_2018-10-24T06Z.rds", "max_flows_2018-10-24T12Z.rds", 
#                     "max_flows_2018-10-24T18Z.rds", "max_flows_2018-10-25T00Z.rds")
# 
# # print(max(as.numeric(site_nwm_max_flows$max_flow), na.rm = TRUE))
# 
# for (f in max_flow_files) {
#   sbtools::item_file_download("5bcf61cde4b0b3fc5cde1742", overwrite_file = TRUE,
#                               names = f, destinations = f)
#   
#   site_nwm_max_flows_n <- readRDS(f)
#   
#   site_nwm_max_flows$max_flow <- pmax(as.numeric(site_nwm_max_flows_n$max_flow), as.numeric(site_nwm_max_flows$max_flow), na.rm = TRUE)
#   
#   # print(max(as.numeric(site_nwm_max_flows_n$max_flow), na.rm = TRUE))
# }

# print(max(site_nwm_max_flows$max_flow, na.rm = TRUE))

nws_flood_stage_list <- jsonlite::fromJSON("https://waterwatch.usgs.gov/webservices/floodstage?format=json")
nws_flood_stage_table <- nws_flood_stage_list[["sites"]]

sbtools::item_file_download("5bcf61cde4b0b3fc5cde1742", overwrite_file = TRUE,
                            names = "FPSSites20181023.xlsx", destinations = "FPSSites20181023.xlsx")

priority_list <- readxl::read_xlsx("FPSSites20181023.xlsx")

siteInfo <- siteInfo %>%
  left_join(select(site_nwm_max_flows, site_no=site, max_flow, `75%`, `95%`, `99%`,`100%`), by="site_no") %>%
  mutate(max_flow = as.numeric(max_flow),
         is_above_75 = max_flow > `75%`,
         is_above_95 = max_flow > `95%`,
         is_above_99 = max_flow >= `99%`,
         is_above_100 = max_flow > `100%`,
         NWS = site_no %in% nws_flood_stage_table$site_no,
         priority = site_no %in% priority_list$SiteNumber) 

################################
# Get QPF data
################################

sbtools::item_file_download("5bcf61cde4b0b3fc5cde1742", names = "qpf.rds", destinations = "qpf.rds", overwrite_file = TRUE)

qpf <- readRDS("qpf.rds")

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

##################################
# Do counts at the end:
##################################
x <- siteInfo %>%
  filter(type == levels(type)[1]) %>%
  group_by(site_tp_cd) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

levels(siteInfo$type)[levels(siteInfo$type)=="Surface Water"] <- paste0(paste0("Surface Water (",sum(siteInfo$site_tp_cd %in% Surface_Water, na.rm = TRUE),")\n  - "),paste0(paste0(x$site_tp_cd, " (", x$count,")"), collapse = "\n  - "))
levels(siteInfo$type)[levels(siteInfo$type)=="Groundwater"] <- paste0("Groundwater (",sum(siteInfo$site_tp_cd %in% Groundwater, na.rm = TRUE),")")
levels(siteInfo$type)[levels(siteInfo$type)=="Spring"] <- paste0("Spring (",sum(siteInfo$site_tp_cd %in% Spring, na.rm = TRUE),")")
levels(siteInfo$type)[levels(siteInfo$type)=="Atmospheric"] <- paste0("Atmospheric (",sum(siteInfo$site_tp_cd %in% Atmospheric, na.rm = TRUE),")")
levels(siteInfo$type)[levels(siteInfo$type)=="Other"] <- paste0("Other (",sum(!(siteInfo$site_tp_cd %in% c(Surface_Water, Groundwater, Spring, Atmospheric)), na.rm = TRUE),")")
sites.df$type <- siteInfo$type

sites.df$NWS <- siteInfo$NWS
sites.df$above <- "<75"
sites.df$above[is.na(siteInfo$max_flow)] <- "Unknown"
sites.df$above[siteInfo$is_above_99] <- ">=99"
sites.df$above[sites.df$above == "<75" & siteInfo$is_above_95] <- "95-98"
sites.df$above[sites.df$above == "<75" & siteInfo$is_above_75] <- "75-95"

sites.df$above <- factor(sites.df$above, levels = c("<75", "75-95", "95-98", ">=99", "Unknown"))


# sites.df$priority <- siteInfo$priority
# sites.df$priority <- ifelse(sites.df$priority, 
#                             paste0("Federal Priority (",sum(siteInfo$priority),")"),
#                             paste0("Other (",sum(!siteInfo$priority),")"))


################################
# Clip QPF to CONUS
################################

qpf <- sf::st_intersection(qpf, sf::st_buffer(sf::st_as_sf(conus), 0))

################################
# Plot it up
################################
# Shapes by site type:
sw_sites <- filter(sites.df, type == levels(sites.df$type)[1])
ahps_sites_total <- sum(sw_sites$NWS)
non_ahps_sites_total <- sum(!sw_sites$NWS)
sw_sites$NWS <- ifelse(sw_sites$NWS, paste0("AHPS site (",ahps_sites_total,")"),
                       paste0("Non-AHPS site (",non_ahps_sites_total,")"))
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
  geom_point(data = sw_sites, size = 2, #alpha = 0.8,
             aes(x = coords.x1, y=coords.x2, 
                 color = NWS, shape = type)) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjus = 0.5),
        legend.justification = "top",
        plot.caption=element_text(hjust=1)) +
  ggtitle(label = paste("Surface Water Site Outage Summary", Sys.time()), subtitle = paste(nrow(sw_sites), "surface water sites currently impacted")) +
  guides(shape = guide_legend(title=NULL, order = 2), 
         color = guide_legend(title=NULL, order = 1),
         size = guide_legend(title = "National Water\nModel Predictions", order = 3)) + 
  labs(caption = "Quantitative Precipitation Forecast (QPF) Valid: 12Z 2018-11-03 Thru 12Z 2018-11-10\n")

gsMap

ggsave(gsMap, filename = "site_outages_type.pdf", width = 11, height = 7)
ggsave(gsMap, filename = "site_outages_type.png", width = 11, height = 7)


# Color by predicted levels:

levels(sw_sites$above)[levels(sw_sites$above) == "<75"] <- paste0("< 75th percentile (",sum(sw_sites$above == "<75"),")")
levels(sw_sites$above)[levels(sw_sites$above) == "75-95"] <- paste0("75th - 94th percentile (",sum(sw_sites$above == "75-95"),")")
levels(sw_sites$above)[levels(sw_sites$above) == "Unknown"] <- paste0("Unknown (",sum(sw_sites$above == "Unknown"),")")
levels(sw_sites$above)[levels(sw_sites$above) == ">=99"] <- paste0("> 99th percentile (",sum(sw_sites$above == ">=99"),")")
levels(sw_sites$above)[levels(sw_sites$above) == "95-98"] <- paste0("95th - 99th percentile (",sum(sw_sites$above == "95-98"),")")

set_colors <- c("darkolivegreen3","steelblue", "yellow","red", "grey")
names(set_colors) <- levels(sw_sites$above)

gsMap_predict <- ggplot() +
  geom_polygon(aes(x = long, y = lat, group = group),
               data = states.out, fill = "grey90",
               alpha = 0.9, color = "grey") +
  geom_sf(aes(fill = QPF), data = qpf, alpha = 0.5, color = "transparent") +
  scale_fill_gradient("WPC 7-DAY QPF\n[inches]", low = "#f2f2f2", high = "#4186f4") +
  coord_sf(datum=NA) +
  geom_polygon(aes(x = long, y = lat, group = group),
               data = states.out, fill = NA,
               alpha = 0.9, color = "grey") +
  geom_point(data = filter(sw_sites, above == levels(sw_sites$above)[5]), size = 2,  
             aes(x = coords.x1, y=coords.x2, 
                 color = above)) +
  geom_point(data = filter(sw_sites, above == levels(sw_sites$above)[1]), size = 2,  
             aes(x = coords.x1, y=coords.x2, 
                 color = above)) +
  geom_point(data = filter(sw_sites, above == levels(sw_sites$above)[2]), size = 2, 
             aes(x = coords.x1, y=coords.x2, 
                 color = above)) +
  geom_point(data = filter(sw_sites, above == levels(sw_sites$above)[3]), size = 2, 
             aes(x = coords.x1, y=coords.x2, 
                 color = above)) +
  geom_point(data = filter(sw_sites, above == levels(sw_sites$above)[4]), size = 2, 
             aes(x = coords.x1, y=coords.x2, 
                 color = above)) +
  scale_color_manual(values = set_colors, breaks = levels(sw_sites$above)) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        plot.title = element_text(hjust = 0.5),
        legend.text = element_text(hjust=0, vjust = 0.5),
        plot.caption = element_text(hjust = 1),
        plot.subtitle = element_text(hjus = 0.5)) +
  ggtitle(label = paste("Surface Water Site Outage Summary", Sys.time()), subtitle = paste(nrow(sw_sites), "surface water sites currently impacted")) +
  guides(shape = guide_legend(title=NULL, order = 2), 
         color = guide_legend(title="National Water Model\n10-day Forecast\nPredicted to Exceed Period of Record\n(based on 1993-2017 hourly retrospective)", order = 1)) +
  labs(caption = "Quantitative Precipitation Forecast (QPF) Valid: 12Z 2018-11-03 THRU 12Z 2018-11-10\nNWM forecasts from 00Z 11-03")

gsMap_predict
ggsave(gsMap_predict, filename = "site_outages_predict.pdf", width = 11, height = 7)
ggsave(gsMap_predict, filename = "site_outages_predict.png", width = 11, height = 7)

