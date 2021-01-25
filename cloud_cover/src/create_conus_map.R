create_conus_map <- function(proj) {
  maps::map('state', fill=TRUE, plot = FALSE) %>%
    st_as_sf(crs = "+proj=longlat +datum=WGS84") %>%
    st_transform(proj)
}
