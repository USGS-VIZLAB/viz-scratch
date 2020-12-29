map_clouds <- function(target_name, date, conus_sf, clouds_raster_fn, cloud_col, frame_height, frame_width){
  
  cloud_cover <- readRDS(clouds_raster_fn)
  
  png(target_name, width = frame_width, height = frame_height)
  plot(st_geometry(conus_sf))
  plot(cloud_cover, add=TRUE, col = cloud_col, legend = FALSE)
  title(sprintf("Cloud cover for %s", date), cex.main = 2.5)
  dev.off()
  
}
