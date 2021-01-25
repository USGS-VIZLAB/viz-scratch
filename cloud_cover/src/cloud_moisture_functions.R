
fetch_cloud_moisture <- function(target_name, date_chr) {
  
  # TODO: add test that you are on VPN
  
  # Using Cloud and Moisture Imagery product from https://www.ncdc.noaa.gov/data-access/satellite-data/goes-r-series-satellites#ACM
  # Access patterns are based on details shared in http://home.chpc.utah.edu/~u0553130/Brian_Blaylock/cgi-bin/goes16_download.cgi?source=aws&satellite=noaa-goes16&domain=C&product=ABI-L2-ACM&date=2020-12-28&hour=0
  date <- as.Date(date_chr)
  product_nm <- 'ABI-L2-CMIPC'
  obj_year <- format(date, "%Y")
  obj_doy <- format(date, "%j")
  obj_hour <- "00" # Doing daily only right now (so start hour at 00)
  
  # There are multiple files per day and the example online just downloads the first one
  obj_prefix <- sprintf('%s/%s/%s/%s', product_nm, obj_year, obj_doy, obj_hour)
  s3_obj <- aws.s3::get_bucket_df('noaa-goes16', prefix = obj_prefix) %>%
    separate(col = "Key", into = c("product_nm", "year", "doy", "hour", "filename"), sep = "/") %>%
    group_by(product_nm, year, doy, hour) %>%
    summarize(nc_file = filename[1], .groups = "keep") %>%
    pull(nc_file)
  
  aws.s3::save_object(
    object = sprintf("%s/%s", obj_prefix, s3_obj),
    bucket = 'noaa-goes16',
    file = target_name, check_region = FALSE)
  
}

process_cloud_moisture_raster <- function(target_name, cloud_data_fn, conus_sf, proj, crop_to_exact_shape = FALSE) {
  
  # Using the var 'CMI' which is 0 to 1 indicating the reflectance factor
  # https://www.ncdc.noaa.gov/data-access/satellite-data/goes-r-series-satellites#CMIP
  
  cloud_nc <- nc_open(cloud_data_fn)
  
  # Turn into raster
  cloud_raster <- raster::raster(ncvar_get(cloud_nc, 'CMI'))
  
  # Set basic CRS
  base_crs <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84"
  crs(cloud_raster) <- base_crs
  
  # Convert rasters of 0 to NA since that value indicates no clouds
  cloud_raster[cloud_raster <= 0.1] <- NA
  
  # Transform to correct projection for viz
  #   Lat/long are originally just numbered, so need to use appropriate extents
  nc_extents <- ncatt_get(cloud_nc, 'geospatial_lat_lon_extent')
  cloud_extents <- extent(raster(
    xmn=nc_extents$geospatial_westbound_longitude,
    ymn=nc_extents$geospatial_southbound_latitude,
    xmx=nc_extents$geospatial_eastbound_longitude,
    ymx=nc_extents$geospatial_northbound_latitude))
  extent(cloud_raster) <- cloud_extents
  
  # Convert conus to this proj to crop then reproject raster
  conus_sf_proj <- st_transform(conus_sf, base_crs)
  
  # Crop to correct geom
  if(crop_to_exact_shape) {
    # Use mask not crop to get specific shape, not just box
    cloud_raster_crop <- mask(cloud_raster, conus_sf_proj) 
  } else {
    cloud_raster_crop <- crop(cloud_raster, conus_sf_proj) 
  }
  
  cloud_raster_crop_proj <- projectRaster(cloud_raster_crop, crs=proj)
  
  saveRDS(cloud_raster_crop_proj, target_name)
  
  nc_close(cloud_nc)
}

map_cloud_moisture <- function(target_name, date, conus_sf, clouds_raster_fn, frame_height, frame_width){
  
  cloud_cover <- readRDS(clouds_raster_fn)
  
  cuts <- seq(0, 1.0, by=0.1) #set breaks
  pal <- colorRampPalette(c("#FFFFFF","#000000"))
  
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
  
  plot(cloud_cover, add=TRUE, breaks = cuts, col = sprintf("%sCC", pal(length(cuts))), legend = FALSE)
  title(sprintf("Cloud moisture for %s", date), cex.main = 2.5, line = -4)
  dev.off()
  
}

