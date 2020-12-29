
fetch_cloud_cover <- function(target_name, date_chr) {
  
  # TODO: add test that you are on VPN
  
  # Using Clear sky product from https://www.ncdc.noaa.gov/data-access/satellite-data/goes-r-series-satellites#ACM
  # Access patterns are based on details shared in http://home.chpc.utah.edu/~u0553130/Brian_Blaylock/cgi-bin/goes16_download.cgi?source=aws&satellite=noaa-goes16&domain=C&product=ABI-L2-ACM&date=2020-12-28&hour=0
  date <- as.Date(date_chr)
  product_nm <- 'ABI-L2-ACMC'
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
