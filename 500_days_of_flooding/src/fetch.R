# Get join NWS flood stage table
get_nws_flood_info <- function() {
  nws_flood_stage_list <- jsonlite::fromJSON("https://waterwatch.usgs.gov/webservices/floodstage?format=json")
  return(nws_flood_stage_list[["sites"]])
}

get_nwis_data <- function(sites, pcode, startDate) {
  readNWISdv(sites, parameterCd = pcode, startDate = startDate) %>% 
      renameNWISColumns()
}

identify_ref_sites <- function(soi_info, earliest_date_to_use, flood_info) {

  # Use drainage area to find sites that are similar to the soi
  soi_drain_area <- soi_info$drain_area_va
  min_drain_area <- soi_drain_area - soi_drain_area*0.25
  max_drain_area <- soi_drain_area + soi_drain_area*0.25
  
  # Cycle through each HUC02 to figure out if there are any sites
  purrr::map(sprintf("%02d", 1:21), function(huc) {
    message(sprintf("Trying HUC %s ...", huc))
    # Some HUCs fail bc of no data to return
    tryCatch(
      whatNWISsites(huc = huc, parameterCd = c("00065"), startDate = earliest_date_to_use,
                  drainAreaMin = min_drain_area, drainAreaMax = max_drain_area) %>% 
      select(site_no, station_nm, dec_lat_va, dec_long_va),
      error = function(e) return()
    )
  }) %>%
    bind_rows() %>%
    # Remove the current soi from this list
    filter(site_no != soi_info$site_no) %>%  
    # Only keep sites that have flood stage available
    left_join(flood_info, by='site_no') %>% 
    filter(!is.na(flood_stage)) 
}
