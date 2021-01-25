map_clouds <- function(target_name, date, conus_sf, clouds_raster_fn, cloud_col, frame_height, frame_width){
  
  cloud_cover <- readRDS(clouds_raster_fn)
  
  png(target_name, width = frame_width, height = frame_height, type = "cairo")
  par(omi = c(0,0,0,0), mai = c(0,0,0,0))
  plot(st_geometry(conus_sf), xaxs = 'i', yaxs = 'i')
  
  # Add after conus_sf is plot, but before clouds (~ line 6-7) 
  this_date_sites <- readRDS("in/site_loc_ready.rds") %>% 
    left_join(filter(readRDS("in/site_data_ready.rds"), 
                     Date == as.Date(date)), by = "site_no")
  plot(st_geometry(this_date_sites), add = TRUE, bg = this_date_sites$bg, 
       pch = this_date_sites$pch, cex = this_date_sites$cex, 
       col = this_date_sites$col, lwd = this_date_sites$lwd)
  
  plot(cloud_cover, add=TRUE, col = cloud_col, legend = FALSE)
  title(sprintf("Cloud cover for %s", date), cex.main = 2.5, line = -4)
  dev.off()
  
}
