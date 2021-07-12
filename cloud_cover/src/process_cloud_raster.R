process_cloud_raster <- function(target_name, cloud_data_fn, conus_sf, proj, crop_to_exact_shape = FALSE) {
  
  # Using the var 'BCM' which is 0 for clear/probably clear or 1 for cloudy/probably cloudy
  # https://www.ncdc.noaa.gov/data-access/satellite-data/goes-r-series-satellites#ACM
  
  cloud_nc <- nc_open(cloud_data_fn)
  
  # Turn into raster
  cloud_raster <- raster::raster(ncvar_get(cloud_nc, 'BCM'))
  
  # Set basic CRS
  crs(cloud_raster) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84"
  
  # Convert rasters of 0 to NA since that value indicates no clouds
  cloud_raster[cloud_raster == 0] <- NA
  
  # Transform to correct projection for viz
  #   Lat/long are originally just numbered, so need to use appropriate extents
  nc_extents <- ncatt_get(cloud_nc, 'geospatial_lat_lon_extent')
  cloud_extents <- extent(raster(
    xmn=nc_extents$geospatial_westbound_longitude,
    ymn=nc_extents$geospatial_southbound_latitude,
    xmx=nc_extents$geospatial_eastbound_longitude,
    ymx=nc_extents$geospatial_northbound_latitude))
  extent(cloud_raster) <- cloud_extents
  cloud_raster_proj <- projectRaster(cloud_raster, crs=proj)
  
  # Crop to correct geom
  if(crop_to_exact_shape) {
    # Use mask not crop to get specific shape, not just box
    cloud_raster_proj_crop <- mask(cloud_raster_proj, conus_sf) 
  } else {
    cloud_raster_proj_crop <- crop(cloud_raster_proj, conus_sf) 
  }
  
  saveRDS(cloud_raster_proj_crop, target_name)
  
  nc_close(cloud_nc)
}
