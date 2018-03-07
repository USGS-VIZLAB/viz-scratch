# mostly from https://owi.usgs.gov/blog/stats-service-map/


dv_sites <- function(){
  
  hucs <- stringr::str_pad(1:21, width = 2, pad = "0")
  
  sites <- c()
  for(huc in hucs){
    sites <- whatNWISdata(huc = huc, service = "dv", startDate = "2018-01-01",
                          parameterCd = "00060", statCd = "00003") %>% 
      .$site_no %>% 
      c(sites)
  }
  return(sites)
}

site_stats <- function(sites){
  
  reqBks <- seq(1, length(sites),by=10)
  statData <- data.frame()
  for(i in reqBks) {
    getSites <- sites[i:(i+9)]
    currentSites <- readNWISstat(siteNumbers = getSites,
                                 parameterCd = "00060", 
                                 statReportType="daily",
                                 statType=c("P05", "P10", "P20", "P25", "P50", "P75", "P80", "P90", "P95","mean"))
    statData <- rbind(statData,currentSites)
  }
  
  return(statData)
}



dv_data <- function(sites, date){
  dv_sites_data <- sapply(sites, FUN = function(x){
    d <- renameNWISColumns(
      readNWISdata(service="dv",
                   site = x,
                   parameterCd="00060",
                   startDate = date,
                   endDate = date))
    if (!is.null(d$Flow)){
      d$Flow
    } else {
      NA
    }
  })
  
  return(dv_sites_data)
  
}


dv_stats <- function(dv_data, site_stats, viz.date){
  
  get_dv <- function(site_no){
    out <- rep(NA, length(site_no))
    for (i in 1:length(site_no)){
      val <- dv_data[[site_no[i]]][1L]
      if (!is.null(val)){
        out[i] <- val
      }
    }
    return(out)
  }
  
  int_per <- function(p05_va, p10_va, p20_va, p25_va, p50_va, p75_va, p80_va, p95_va, dv_val){
    out <- rep(NA, length(dv_val))
    
    
    for (i in 1:length(out)){
      y <- c(0.05, 0.1, 0.2, 0.25, 0.5, 0.75, 0.8, 0.95)
      x <- c(p05_va[i], p10_va[i], p20_va[i], p25_va[i], p50_va[i], p75_va[i], p80_va[i], p95_va[i]) %>% as.numeric
      nas <- is.na(x)
      x <- x[!nas]
      y <- y[!nas]
      if (length(unique(x)) < 2){
        out[i] <- NA
      } else if (dv_val[i] < x[1L]){ # the first and last *have* to be numbers per filtering criteria
        out[i] <- 0.05
      } else if (dv_val[i] > tail(x, 1L)){
        out[i] <- 0.95
      } else {
        out[i] <- approx(x, y, xout = dv_val[i])$y
      }
    }
    return(out)
    
  }
  
  site_stats_viz <- site_stats[site_stats$month_nu == month(viz.date) & 
                               site_stats$day_nu == day(viz.date),] %>% 
    mutate(dv_val = get_dv(site_no)) %>% 
    filter(!is.na(p05_va), !is.na(p95_va), !is.na(dv_val)) %>% 
    mutate(per = int_per(p05_va, p10_va, p20_va, p25_va, p50_va, p75_va, p80_va, p95_va, dv_val)) %>% 
    select(site_no, per)
    
  return(site_stats_viz)
  
}
