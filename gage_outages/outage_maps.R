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

# sbtools::authenticate_sb()
vizlab::authRemote('sciencebase')
# Site from google sheet at 6:30am Thursday 10/25:
# sites <- setDF(fread("site_list_thurs_morning.tsv", sep = "\t"))
# 
# sites$siteID_15 <- str_match( sites[[1]], "\\d{15}")[,1]
# sites$siteID_10 <- str_match( sites[[1]], "\\d{10}")[,1]
# sites$siteID_9 <- str_match( sites[[1]], "\\d{9}")[,1]
# sites$siteID_8 <- str_match( sites[[1]], "\\d{8}")[,1]
# 
# sites$siteID <- sites$siteID_15
# sites$siteID[is.na(sites$siteID)] <- sites$siteID_10[is.na(sites$siteID)]
# sites$siteID[is.na(sites$siteID)] <- sites$siteID_9[is.na(sites$siteID)]
# sites$siteID[is.na(sites$siteID)] <- sites$siteID_8[is.na(sites$siteID)]
# 
# fwrite(sites, "filtered_sites_thurs.csv")


################################
# Get QPF data
################################

sbtools::item_file_download("5bcf61cde4b0b3fc5cde1742", names = "qpf.rds", destinations = "qpf.rds", overwrite_file = TRUE)

qpf <- readRDS("qpf.rds")

################################
# Get NWM Max Flows
################################

latest_m_flows <- "max_flows_2018-10-25T00Z.rds"

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

################################
# Get NWIS Data
################################
# 
# sites <- fread("filtered_sites_thurs.csv", colClasses = "character")
# sites <- filter(sites, nchar(siteID) > 0)
# 
# setAccess("internal")
# 
# siteInfo <- readNWISsite(sites$siteID)
# 
# sites_with_NWIS <- sites %>%
#   select(site_no = siteID) %>% # removed , GAGE = `GAGE Site`
#   left_join(siteInfo, by = "site_no")
# 
# fwrite(sites_with_NWIS, "sites_NWIS_thurs.csv")
# 
# sbtools::item_append_files("5bcf61cde4b0b3fc5cde1742",
#                             files = "sites_NWIS_thurs.csv", destinations = "sites_NWIS_thurs.csv")

sbtools::item_file_download("5bcf61cde4b0b3fc5cde1742", overwrite_file = TRUE,
                            names = "sites_NWIS_thurs.csv", destinations = "sites_NWIS.csv")
siteInfo <- fread("sites_NWIS.csv", colClasses = c(site_no = "character"))

nws_flood_stage_list <- jsonlite::fromJSON("https://waterwatch.usgs.gov/webservices/floodstage?format=json")
nws_flood_stage_table <- nws_flood_stage_list[["sites"]]

sbtools::item_file_download("5bcf61cde4b0b3fc5cde1742", overwrite_file = TRUE,
                            names = "FPSSites20181023.xlsx", destinations = "FPSSites20181023.xlsx")

priority_list <- readxl::read_xlsx("FPSSites20181023.xlsx")

siteInfo <- siteInfo %>% 
  filter(!(duplicated(site_no)))

Surface_Water <- c("ES","LK","OC","OC-CO","ST","ST-CA","ST-DCH","ST-TS","WE")
Groundwater <- c("GW","GW-CR", "GW-EX","GW-HZ","GW-IW","GW-MW","GW-TH",
                 "SB","SB-CV","SB-GWD","SB-TSM","SB-UZ")
Spring <- c("SP")
Atmospheric <- "AT"

siteInfo <- siteInfo %>%
  select(site_no, dec_lat_va, dec_long_va, state_cd, site_tp_cd)  %>%
  left_join(select(site_nwm_max_flows, site_no=site, max_flow, `75%`, `95%`, `99%`,`100%`), by="site_no") %>%
  mutate(max_flow = as.numeric(max_flow),
         is_above_75 = max_flow > `75%`,
         is_above_95 = max_flow > `95%`,
         is_above_99 = max_flow >= `99%`,
         is_above_100 = max_flow > `100%`,
         NWS = site_no %in% nws_flood_stage_table$site_no,
         priority = site_no %in% priority_list$SiteNumber) %>%
  filter(!is.na(dec_lat_va))

siteInfo$state[!is.na(siteInfo$state_cd)]  = dataRetrieval::stateCdLookup(siteInfo$state_cd[!is.na(siteInfo$state_cd)], "postal")

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

# Ignore the 1 in Guam:
siteInfo <- filter(siteInfo, state != "GU")

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

sites.df$NWS <- siteInfo$NWS
sites.df$priority <- siteInfo$priority

sites.df$above <- "<75"
sites.df$above[is.na(siteInfo$max_flow)] <- "Unknown"
sites.df$above[siteInfo$is_above_99] <- ">=99"
sites.df$above[sites.df$above == "<75" & siteInfo$is_above_95] <- "95-98"
sites.df$above[sites.df$above == "<75" & siteInfo$is_above_75] <- "75-95"

sites.df$above <- factor(sites.df$above, levels = c("<75", "75-95", "95-98", ">=99", "Unknown"))

levels(sites.df$above)[levels(sites.df$above) == "<75"] <- paste0("< 75th percentile (",sum(sites.df$above == "<75"),")")
levels(sites.df$above)[levels(sites.df$above) == "75-95"] <- paste0("75th - 94th percentile (",sum(sites.df$above == "75-95"),")")
levels(sites.df$above)[levels(sites.df$above) == "Unknown"] <- paste0("Unknown (",sum(sites.df$above == "Unknown"),")")
levels(sites.df$above)[levels(sites.df$above) == ">=99"] <- paste0("> 99th percentile (",sum(sites.df$above == ">=99"),")")
levels(sites.df$above)[levels(sites.df$above) == "95-98"] <- paste0("95th - 99th percentile (",sum(sites.df$above == "95-98"),")")

sites.df$type <- siteInfo$type

sites.df$NWS <- ifelse(sites.df$NWS, paste0("AHPS site (",sum(siteInfo$NWS),")"),
                       paste0("Non-AHPS site (",sum(!siteInfo$NWS),")"))

# sites.df$priority <- ifelse(sites.df$priority, 
#                             paste0("Federal Priority (",sum(siteInfo$priority),")"),
#                             paste0("Other (",sum(!siteInfo$priority),")"))

# sites.df$above[is.na(sites.df$above)] <- paste0("Unknown (",sum(is.na(sites.df$above)),")")
# 
# sites.df$above <- ifelse(sites.df$above == "TRUE",
#                          paste0("Above 95% (",sum(sites.df$above == "TRUE", na.rm = TRUE),")"),
#                          paste0("Below 95% (",sum(!sites.df$above == "TRUE", na.rm = TRUE),")"))

################################
# Clip QPF to CONUS
################################

qpf <- sf::st_intersection(qpf, sf::st_buffer(sf::st_as_sf(conus), 0))

################################
# Plot it up
################################
# Shapes by site type:
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
  # geom_point(data = filter(sites.df, above),
  #            size = 3, color = "black",
  #            aes(x = coords.x1, y=coords.x2,
  #                shape = priority)) +
  geom_point(data = sites.df, size = 2, #alpha = 0.8,
             aes(x = coords.x1, y=coords.x2, 
                 color = NWS, shape = type)) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjus = 0.5)) +
  ggtitle(label = paste("Site Outage Summary", Sys.Date()), subtitle = paste(nrow(siteInfo), "sites currently impacted")) +
  guides(shape = guide_legend(title=NULL, order = 2), 
         color = guide_legend(title=NULL, order = 1),
         size = guide_legend(title = "National Water\nModel Predictions", order = 3)) + 
  labs(caption = "         Quantitative Precipitation Forecast (QPF) VALID: 12Z 2018-10-25 THRU 12Z 2018-01-01\n")

gsMap
ggsave(gsMap, filename = "site_outages_type.pdf", width = 11, height = 7)
ggsave(gsMap, filename = "site_outages_type.png", width = 11, height = 7)

# Color by predicted levels:
sw <- filter(sites.df, type == levels(sites.df$type)[1])
set_colors <- c("darkolivegreen3","steelblue", "yellow",
                "red", "grey")
names(set_colors) <- levels(sw$above)

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
  geom_point(data = filter(sw, above == levels(sw$above)[5]), size = 2,  
             aes(x = coords.x1, y=coords.x2, 
                 color = above)) +
  geom_point(data = filter(sw, above == levels(sw$above)[1]), size = 2,  
             aes(x = coords.x1, y=coords.x2, 
                 color = above)) +
  geom_point(data = filter(sw, above == levels(sw$above)[2]), size = 2, 
             aes(x = coords.x1, y=coords.x2, 
                 color = above)) +
  geom_point(data = filter(sw, above == levels(sw$above)[3]), size = 2, 
             aes(x = coords.x1, y=coords.x2, 
                 color = above)) +
  geom_point(data = filter(sw, above == levels(sw$above)[4]), size = 2, 
             aes(x = coords.x1, y=coords.x2, 
                 color = above)) +
  scale_color_manual(values = set_colors, breaks = levels(sw$above)) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        legend.text = element_text(hjust=0, vjust = 0.5),
        plot.caption = element_text(hjust = 0)) +
  ggtitle(label = paste("Streamgage Outage Summary", Sys.Date())) +
  guides(shape = guide_legend(title=NULL, order = 2), 
         color = guide_legend(title="National Water Model\n10-day Forecast\nPredicted to Exceed Period of Record\n(based on 1993-2017 hourly retrospective)", order = 1)) +
  labs(caption = "         Quantitative Precipitation Forecast (QPF) VALID: 12Z 2018-10-25 THRU 12Z 2018-01-01\n         NWM forecasts from 00Z 10-25")

# VALID: 12Z 2018-10-25 THRU 12Z 2018-01-01

gsMap_predict
ggsave(gsMap_predict, filename = "site_outages_predict.pdf", width = 11, height = 7)
ggsave(gsMap_predict, filename = "site_outages_predict.png", width = 11, height = 7)

