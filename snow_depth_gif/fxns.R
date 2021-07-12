# Functions used to build a simple snow depth animation

fetch_snow_data <- function(date_format, snow_dir) {
  
  ymd_str <- format(date_format, "%Y%m%d") 
  fn_out <- sprintf("%s/snow_raw_%s.dat", snow_dir, ymd_str)
  
  # construct URL
  year <- format(date_format, "%Y")
  month <- format(date_format, "%m")
  month_str <- month.abb[as.numeric(month)]
  
  # Identify download location & tmp save location
  tmp_dir <- sprintf("%s/tmp_dl", snow_dir)
  if(!dir.exists(tmp_dir)) dir.create(tmp_dir)
  ftp_base <- "ftp://sidads.colorado.edu/DATASETS/NOAA/G02158/unmasked/%s/%s"
  ftp_folder <- sprintf(ftp_base, year, paste(month, month_str, sep = "_"))
  url_tar <- sprintf('%s/SNODAS_unmasked_%s.tar', ftp_folder, ymd_str)
  fn_tar <- file.path(tmp_dir, sprintf("%s.tar", ymd_str))
  
  # Download and unzip the snow data
  download.file(url_tar, destfile = fn_tar, mode = "wb")
  untar(fn_tar, exdir = tmp_dir)
  
  # Find the snow depth file and unzip
  fn_tmp <- list.files(tmp_dir, full.names = TRUE)
  fn_snow <- fn_tmp[grep("zz_ssmv11036", fn_tmp)]
  fn_snow_dat <- fn_snow[grep(".dat", fn_snow)]
  fn_zip <- fn_snow_dat[grep(ymd_str, fn_snow_dat)]
  R.utils::gunzip(fn_zip, destname = fn_out, overwrite = TRUE)
  
  return(fn_out)
}

process_snow_raster <- function(snow_data_fn, states_sf) {
  
  # Size of SNODAS data
  n_col <- 8192
  n_row <- 4096
  
  # boundary extent for unmasked SNODAS
  x0 <- -130.516666666661
  x1 <- -62.2499999999975
  y0 <- 24.0999999999990
  y1 <- 58.2333333333310
  
  snow_depth <- readBin(snow_data_fn, integer(),
                        n=n_row*n_col, size=2, signed=TRUE, endian='big')
  snow_depth[snow_depth <= 0] <- 0
  snow_depth_mat <- matrix(snow_depth, nrow = n_col)
  
  # Convert to raster
  snow_raster <- raster::raster(ncol=n_col, nrow=n_row, xmn = x0, xmx = x1, ymn = y0, ymx = y1)
  snow_raster <- raster::setValues(snow_raster, snow_depth)
  
  # Add the correct projection
  # Source for WGS 84 projection: "Projecting SNODAS Data" section on https://nsidc.org/data/g02158
  raster::crs(snow_raster) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
  
  # Now crop
  snow_raster_crop <- raster::crop(snow_raster, states_sf)
  
  return(snow_raster_crop)
  
}

download_data <- function(dates_to_see, states_sf, snow_dir) {
  raster_fn_all <- c()
  for(date_i in seq_along(dates_to_see)) {
    raster_fn_out <- sprintf("%s/snow_ready_%s.rds", snow_dir, format(dates_to_see[date_i], "%Y%m%d"))
    if(!file.exists(raster_fn_out)) {
      date_raw_snow_fn <- fetch_snow_data(dates_to_see[date_i], snow_dir)
      date_snow_raster <- process_snow_raster(date_raw_snow_fn, states_sf)
      saveRDS(date_snow_raster, raster_fn_out)
    }
    raster_fn_all <- c(raster_fn_all, raster_fn_out)
  }
  return(raster_fn_all)
}

create_frames <- function(dates_to_see, raster_all_fn, snow_dir) {
  
  # Read in and put all raster obj in a list
  snow_raster_all <- lapply(raster_fn_all, readRDS)
  
  # Make plot for each date
  snow_breaks <- seq(0, 1000, by = 200)
  snow_cols <- rev(scico::scico(length(snow_breaks), palette = 'devon'))
  
  frames_all <- c()
  for(date_i in seq_along(dates_to_see)) {
    frame_nm <- sprintf("%s/frame_snow_%s.png", snow_dir, format(dates_to_see[date_i], "%Y%m%d"))
    png(frame_nm)
    par(omi=c(0,0,0,0), mar=c(0,1,0,1), bty="n")
    plot(snow_raster_all[[date_i]], axes=FALSE, breaks = snow_breaks, col = snow_cols)
    plot(st_geometry(states_sf), add=TRUE)
    mtext(dates_to_see[date_i], line = -4)
    dev.off()
    frames_all <- c(frames_all, frame_nm)
  }
  
  return(frames_all)
}

make_gif <- function(anim_fn, frames) {
  gif_tmp_dir <- sprintf("%s/magick", dirname(frames[1]))
  if(!dir.exists(gif_tmp_dir)) dir.create(gif_tmp_dir)
  
  # Create a single string of all the frames going to be stitched together
  #   to pass to ImageMagick commans.
  frame_str <- paste(frames, collapse = " ")
  
  # Create the ImageMagick command to convert the files into a GIF
  magick_command <- sprintf(
    'convert -define registry:temporary-path=%s -limit memory 24GiB -delay %s -loop 0 %s %s',
    gif_tmp_dir, 50, frame_str, anim_fn)
  
  # If you are on a Windows machine, you will need to have `magick` added before the command
  if(Sys.info()[['sysname']] == "Windows") {
    magick_command <- sprintf('magick %s', magick_command)
  }
  
  # Now actually run the command and create the GIF
  system(magick_command)
  
  return(anim_fn)
}

