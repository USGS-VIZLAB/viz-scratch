# Port over data from `gage-conditions-gif` to do a quick test
# 1. Copied over `2_process/out/dv_stat_styles.rds` as `in/gage_data_all.rds`
# 2. Copied over `1_fetch/out/site_locations.rds` as `in/gage_loc_all.rds`

library(scipiper)
library(sf)
library(dplyr)

dates <- scmake("dates")
proj <- scmake("proj_str")
conus_sf <- scmake("conus_sf")

# Keep only sites in CONUS
sites_sf <- readRDS("in/gage_loc_all.rds") %>% 
  st_as_sf(coords = c("lon", "lat"), crs = "+proj=longlat +datum=WGS84") %>% 
  st_transform(proj) %>% 
  st_crop(conus_sf)

# Keep only data for this time period & CONUS sites
site_data <- readRDS("in/gage_data_all.rds") %>% 
  mutate(Date = as.Date(dateTime)) %>% 
  filter(Date %in% dates) %>% 
  filter(site_no %in% sites_sf$site_no) %>% 
  mutate(cex = cex*0.5) %>% # have to shrink these so they don't look crowded
  mutate(lwd = lwd*0.5) %>% # have to shrink these so they don't look crowded
  select(site_no, Date, bg, pch, cex, col, lwd)

# Save to use in pipeline
saveRDS(sites_sf, "in/site_loc_ready.rds")
saveRDS(site_data, "in/site_data_ready.rds")

# Then, insert the following into the `map_clouds.R` code. The sites should be
# added to the plot after `conus_sf`, but before the clouds.

# Add after conus_sf is plot, but before clouds (~ line 6-7)
this_date_sites <- readRDS("in/site_loc_ready.rds") %>% 
  left_join(filter(readRDS("in/site_data_ready.rds"), 
                   Date == as.Date(date)), by = "site_no")
plot(st_geometry(this_date_sites), add = TRUE, bg = this_date_sites$bg, 
     pch = this_date_sites$pch, cex = this_date_sites$cex, 
     col = this_date_sites$col, lwd = this_date_sites$lwd)

# Then run `scmake()` and view the video with gage data.