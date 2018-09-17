library(dplyr)

state_cds <- c("NC","SC","GA")
pCodes = c("00065")
dates <- list(start = "2018-09-09 00:00:00")
path_to_save <- "test_data"
  
fetch_sites_from_states <- function(state_cds, dates, pCodes, path_to_save) {
  
  # Cast wide net for all NWIS sites with stage data that fall within that bbox
  sites_df <- dplyr::bind_rows(lapply(state_cds, function(cd) {
    dataRetrieval::whatNWISdata(stateCd = cd, parameterCd = pCodes, service = "uv") %>%
      dplyr::select(site_no, station_nm, dec_lat_va, dec_long_va, site_tp_cd, end_date, begin_date)
  }))
  
  # Get NWS flood stage table
  nws_flood_stage_list <- jsonlite::fromJSON("https://waterwatch.usgs.gov/webservices/floodstage?format=json")
  nws_flood_stage_table <- nws_flood_stage_list[["sites"]]
  
  # Filtering applied to every storm
  sites_filtered <- sites_df %>%
    # Filter out any sites that don't have flood stage data from NWS
    inner_join(nws_flood_stage_table, by='site_no') %>%
    dplyr::filter(!is.na(flood_stage)) %>%
    # we only need stream sites
    dplyr::filter(site_tp_cd == "ST") %>%
    # keeps only sites that have data since the start of the storm
    # if a gage goes out during the storm, this filter would still capture that gage
    # also filter out sites that weren't up before the start of the storm (e.g., we are GIF'ing a historical storm)
    dplyr::filter(end_date >= as.Date(dates$start), begin_date <= as.Date(dates$start))
  
  # Only keep the columns we need
  sites <- dplyr::select(sites_filtered, site_no, station_nm, dec_lat_va, dec_long_va, flood_stage) %>%
    # reduce to distinct sites (why are there duplicates?? but there are)
    distinct()
  
  more_site_info <- dataRetrieval::readNWISsite(sites$site_no)
  
  sites <- dplyr::left_join(sites, dplyr::select(more_site_info, site_no, drain_area_va), by="site_no")
  
  # Write the data file
  sub_folders <- strsplit(path_to_save, "/")[[1]]
  current_folder <- sub_folders[1]
  for(folder in sub_folders[-1]){
    dir.create(path = current_folder,
               showWarnings = FALSE)
    current_folder <- paste(current_folder, folder, sep = "/")
  }
  dir.create(path = current_folder,
             showWarnings = FALSE)
  saveRDS(sites, file.path(path_to_save,"all_sites.rds"))

  all_flow <- dataRetrieval::readNWISuv(siteNumbers = sites$site_no,
                                        parameterCd = pCodes, 
                                        startDate = as.Date(dates$start))
  
  saveRDS(all_flow, file.path(path_to_save,"all_flow.rds"))
  
}

fetch_sites_from_states(state_cds = state_cds,
                        dates = dates,
                        path_to_save = path_to_save,
                        pCodes = pCodes)






