library(googlesheets)
library(dataRetrieval)
library(tidyverse)
library(assertthat)
library(maptools)
title <- gs_title("ORIGINAL -- GOES/DA ISSUE STARTING 2018-10-20")
affected_site_list <- gs_read(title, ws = "Gages", range = "B4:N1200") %>% 
  filter_all(any_vars(!is.na(.)))

affected_site_list$siteID_15 <- stringr::str_match( affected_site_list[[1]], "\\d{15}")[,1]
affected_site_list$siteID_10 <- stringr::str_match( affected_site_list[[1]], "\\d{10}")[,1]
affected_site_list$siteID_9 <- stringr::str_match( affected_site_list[[1]], "\\d{9}")[,1]
affected_site_list$siteID_8 <- stringr::str_match( affected_site_list[[1]], "\\d{8}")[,1]

affected_site_list$siteID <- affected_site_list$siteID_15
affected_site_list$siteID[is.na(affected_site_list$siteID)] <- affected_site_list$siteID_10[is.na(affected_site_list$siteID)]
affected_site_list$siteID[is.na(affected_site_list$siteID)] <- affected_site_list$siteID_9[is.na(affected_site_list$siteID)]
affected_site_list$siteID[is.na(affected_site_list$siteID)] <- affected_site_list$siteID_8[is.na(affected_site_list$siteID)]
#one site had a typo in the id, two are duplicated
affected_site_list_use <- affected_site_list %>% select(-contains("SiteID_"), site_no=siteID) %>% 
  mutate(site_no = ifelse(site_no == "445227068520101", no = site_no, yes = "445227067520101"),
         Affected = "Affected") %>% 
  filter(!duplicated(site_no))
assert_that(all(!is.na(affected_site_list_use$site_no)))





#all real time sites
all_rt_sites <- importRDB1('https://waterdata.usgs.gov/nwis/current?site_tp_cd=AT&site_tp_cd=GL&site_tp_cd=OC&site_tp_cd=OC-CO&site_tp_cd=ES&site_tp_cd=LK&site_tp_cd=ST&site_tp_cd=ST-CA&site_tp_cd=ST-DCH&site_tp_cd=ST-TS&site_tp_cd=SP&site_tp_cd=GW&site_tp_cd=GW-CR&site_tp_cd=GW-EX&site_tp_cd=GW-HZ&site_tp_cd=GW-IW&site_tp_cd=GW-MW&site_tp_cd=GW-TH&site_tp_cd=SB&site_tp_cd=SB-CV&site_tp_cd=SB-GWD&site_tp_cd=SB-TSM&site_tp_cd=SB-UZ&site_tp_cd=WE&site_tp_cd=LA&site_tp_cd=LA-EX&site_tp_cd=LA-OU&site_tp_cd=LA-SNK&site_tp_cd=LA-SH&site_tp_cd=LA-SR&site_tp_cd=FA&site_tp_cd=FA-CI&site_tp_cd=FA-CS&site_tp_cd=FA-DV&site_tp_cd=FA-FON&site_tp_cd=FA-GC&site_tp_cd=FA-HP&site_tp_cd=FA-QC&site_tp_cd=FA-LF&site_tp_cd=FA-OF&site_tp_cd=FA-PV&site_tp_cd=FA-SPS&site_tp_cd=FA-STS&site_tp_cd=FA-TEP&site_tp_cd=FA-WIW&site_tp_cd=FA-SEW&site_tp_cd=FA-WWD&site_tp_cd=FA-WWTP&site_tp_cd=FA-WDS&site_tp_cd=FA-WTP&site_tp_cd=FA-WU&site_tp_cd=AW&site_tp_cd=AG&site_tp_cd=AS&index_pmcode_STATION_NM=1&index_pmcode_DATETIME=2&group_key=NONE&sitefile_output_format=html_table&column_name=agency_cd&column_name=site_no&column_name=station_nm&column_name=dec_lat_va&column_name=dec_long_va&sort_key_2=site_no&html_table_group_key=NONE&format=rdb&rdb_compression=value&list_of_search_criteria=site_tp_cd%2Crealtime_parameter_selection')
all_rt_sites_filtered <- all_rt_sites %>% filter(result_md > '2018-10-01') %>% 
  filter(!duplicated(site_no)) 
#assert_that(all(affected_site_list_use$site_no %in% all_rt_sites_filtered$site_no))
not_included_rt <- slice(affected_site_list, c(719, 735))
#two sites weren't included in the list of all real-time sites
#so need to do a full join 
all_sites <- full_join(all_rt_sites_filtered, affected_site_list_use, by = "site_no")

#dataretrieval to get lat/lons for all sites
#note that there are duplicated sites in readNWISsite output (multi-agency stuff?), so removing duplicates
#dropping sites in Guam and Canada and PR and VI, and three that don't have lat/lons
site_info <- readNWISsite(all_sites$site_no) %>% filter(!duplicated(site_no))
all_sites_locations <- left_join(all_sites, site_info, by = "site_no") %>% 
  filter(!grepl(pattern = ", guam", x = station_nm.y, ignore.case = TRUE) & country_cd != "CA") %>% 
  mutate(state = stateCdLookup(state_cd, outputType = "postal")) %>% 
  filter(!is.na(dec_lat_va) & !is.na(dec_long_va) & !state %in% c("PR", "VI"))

#need to add a state column for shifting AK and HI

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
coords = cbind(all_sites_locations$dec_long_va, all_sites_locations$dec_lat_va)

sites = SpatialPoints(coords, proj4string = CRS(wgs84)) %>%
  spTransform(CRS(proj4string(states.out)))

sites.df <- as.data.frame(sites)

for(i in names(move_variables)){
  shifted <- do.call(shift_sp, c(sp = stuff_to_move[[i]],
                                 move_variables[[i]],
                                 proj.string = proj4string(conus),
                                 row.names = i))
  states.out <- rbind(shifted, states.out, makeUniqueIDs = TRUE)
  
  shifted.sites <- do.call(shift_sp, c(sp = sites[all_sites_locations$state == i,],
                                       move_variables[[i]],
                                       proj.string = proj4string(conus),
                                       ref=stuff_to_move[[i]])) %>%
    as.data.frame %>%
    coordinates()
  
  sites.df[all_sites_locations$state == i, ] <- shifted.sites
  
}

sites.df_plot <- bind_cols(sites.df, all_sites_locations) %>% 
  mutate(Affected = ifelse(is.na(Affected), yes= "Not affected", no = Affected))
########

gsMap <- ggplot() +
  geom_polygon(aes(x = long, y = lat, group = group),
               data = states.out, fill = "grey90",
               alpha = 0.9, color = "grey") +
  coord_sf(datum=NA) +
  geom_polygon(aes(x = long, y = lat, group = group),
               data = states.out, fill = NA,
               alpha = 0.9, color = "grey") +
  geom_point(data = sites.df_plot, size = 0.5, aes(x = coords.x1, y=coords.x2, color = Affected)) +
  theme(panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        panel.background = element_rect(fill = "transparent", color = NA),
        legend.key = element_rect(fill = "transparent", color = NA),
        legend.text=element_text(size=12)) + 
  guides(color=guide_legend(title="")) +
  ggtitle("Site Outage Summary")
ggsave(plot = gsMap, filename = "all_gages_outage.png")
ggsave(plot = gsMap, filename = "all_gages_outage.pdf")


