library(googlesheets)

sbtools::authenticate_sb()
token <- gs_auth(cache = FALSE)
title_2 <- gs_title("GOES/DA ISSUE STARTING 2018-10-20")
current_site_list_raw <- gs_read(title_2, ws = "Gages", range = "A5:AC1000")

latest_m_flows <- "max_flows_2018-11-05T00Z.rds"

qpf_caption <- "Quantitative Precipitation Forecast (QPF) Valid: 12Z 2018-11-05 Thru 12Z 2018-11-12\n"

nwm_caption <-  paste0(qpf_caption, "NWM forecasts from 00Z 11-05")

source("../viz-scratch/gage_outages/outage_maps.R")
source("../viz-scratch/gage_outages/priority_map.R")
source("../viz-scratch/gage_outages/spreadsheet_ranking.R")
