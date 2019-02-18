

munge_ice_data <- function(filein){
  ice_data <- readr::read_csv(filein) %>% 
    rename(start = `START YEAR`, end = `END YEAR`) %>% 
    mutate(close_date = as.Date(paste0(start, " ", CLOSED), format = "%Y %d %b"),
           open_date = as.Date(paste0(end, " ", OPENED), format = "%Y %d %b")) %>% 
    mutate(close_date = as.Date(ifelse(lubridate::month(close_date) < 6, close_date+365, close_date), origin = '1970-01-01')) %>% 
    mutate(open_date = as.Date(ifelse(lubridate::month(open_date) > 11, open_date-365, open_date), origin = '1970-01-01'))
}

