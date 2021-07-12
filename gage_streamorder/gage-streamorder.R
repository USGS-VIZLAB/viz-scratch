# get streamorder of a single gage site  ---------------------
library(dataRetrieval)
library(tidyverse)
library(nhdplusTools)
library(sf)

# for all gages (as of 2019) - thesite_no, number of years active, when started, and any inactive years
gages <- readRDS("data/active_flow_gages_summary_wy.rds")
str(gages)

# make a list of all gages
gage_list <- gages %>% pull(site) %>% unique

## find drainage area from NWIS
readNWISsite(gage_list[1]) # for a single site
gage_da <- readNWISsite(gage_list) # for a all sites

# Pick a random single gage!
rand <- 10000

# grab coords of a single gage and convert to comid for NHDPLus
start <- gage_da %>% slice(rand)
start_point <- st_sfc(st_point(c(start$dec_long_va,start$dec_lat_va)), crs = 4269)
comid <- discover_nhdplus_id(start_point)

# get the next closest comid within 10 km
nldi_feature <- list(featureSource = "comid", featureID = start_comid)
flowline_nldi <- navigate_nldi(nldi_feature, 
                               mode = "upstreamTributaries", 
                               distance_km = 100)
next_comid <- flowline_nldi$UT_flowlines %>% slice(1)

# assign to a variable
test_flowline <- data.frame(
  ID = comid,
  toID = next_comid$nhdplus_comid
)
# get stream order from comid
order <- get_streamorder(test_flowline)

# see if it makes sense
message("Gage ", start$site_no, " has start coordinates of ", start_point," and a comid of
        ",comid,". The next nearest comid is ",next_comid$nhdplus_comid," and between the two points, get_streamorder() returns a value of ", order,".")


# after discussion, we're realizing the streamorder value might change based on spatial scale.
# This script only returns a value of 1 no matter the start point, which might be because we're only giving it a single flowline between two nearby COMIDS so by default, the streamorder is 1.
