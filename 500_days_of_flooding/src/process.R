
# Find minimum date in a daily dataset
get_min_date <- function(nwis_data) {
  min(nwis_data$Date)
}

add_gage_height <- function(nwis_data, rating_curve) {
  # Add GH (DEP = flow, INDEP = gage height)
  nwis_data$GH <- approx(rating_curve$DEP, rating_curve$INDEP, nwis_data$Flow)$y
  # Since GH is derived from Flow in this scenario, have the cd match
  nwis_data$GH_cd <- nwis_data$Flow_cd 
  return(nwis_data)
}

# Using gage height and NWS flood stage, identify when a gage is flooding
identify_flooding <- function(gh_data, flood_info, flood_type = c("flood_stage", "moderate_flood_stage", "major_flood_stage")) {
  gh_data %>% 
    
    left_join(flood_info, by='site_no') %>%
    rename(flood_val = !!flood_type) %>% # Use the desired flood column
    mutate(flood_val = as.numeric(flood_val)) %>% 
    
    # Determine whether the site is flooding
    find_prev_next_open_water_val() %>% 
    mutate(is_flooding = ifelse(
      # If there is ice, use the prev and next open water GH to determine if it is flooding
      # Counting as flood if the non-ice days sandwiching the ice period were in flood
      is_ice, prev_open_water_val >= flood_val & next_open_water_val >= flood_val, 
      # The it is open water & the GH is not NA, use it to determine if there is flooding
      ifelse(!is.na(GH), GH >= flood_val,
             # If there is no ice and GH is missing, then return FALSE for flooding
             FALSE))) %>% 
    
    mutate(year = as.numeric(format(Date, "%Y"))) %>% 
    select(site_no, Date, year, GH, is_flooding)
    
}

find_prev_next_open_water_val <- function(data) {
  data %>% 
    group_by(site_no) %>%
    mutate(is_ice = Flow_cd == "P Ice") %>% 
    # Identify individual ice periods by identifying consecutive ice days and 
    # assigning a number every time the following day does not have "P Ice". 
    # The open water days leading up to an ice period are included.
    mutate(ice_evt = cumsum(c(FALSE, diff(is_ice) < 0))) %>% 
    mutate(row_i = row_number()) %>% 
    
    # Figure out the GH from the day before and day after
    # each ice period.
    group_by(site_no, ice_evt) %>% 
    mutate(start_ice_evt = ifelse(any(is_ice), row_i[min(which(is_ice))], NA),
           end_ice_evt = ifelse(any(is_ice), row_i[max(which(is_ice))], NA)) %>% 
    ungroup() %>% 
    mutate(prev_open_water_val = GH[start_ice_evt-1],
           next_open_water_val = GH[end_ice_evt+1]) %>% 
    select(-ice_evt, -row_i, -start_ice_evt, -end_ice_evt)
}


# Using consecutive days of flooding, determine how many days 
summarize_flood_events <- function(flood_data) {
  flood_data %>%
    group_by(site_no) %>%
    
    # Identify individual flood events by identifying consecutive flooding 
    # days and assigning a number every time the following day is not flooding.
    # This means that the non-flooding days before a flood event are included.
    mutate(flood_evt = cumsum(c(FALSE, diff(is_flooding) < 0))) %>% 
    
    # Add column of cumulative consecutive flooding days for event
    mutate(cumulative_flood_days = count_consecutive_flood_days_per_evt(is_flooding, flood_evt)) %>% 
    
    # Remove days that are not flooding before summarizing event lengths. Would be  
    # confusing because days with no flood could end up with a max flood evt length
    filter(cumulative_flood_days > 0) %>%
    
    # Determine the length of each flood event
    group_by(site_no, flood_evt) %>% 
    mutate(flood_evt_length = max(cumulative_flood_days)) %>% 
    ungroup()
}

# Use this to accumulate flood days for each event
count_consecutive_flood_days_per_evt <- function(bool_col, grp) {
  cumul_days <- unlist(lapply(split(bool_col, grp), cumsum))
  names(cumul_days) <- NULL
  return(cumul_days)
}

# For each site, identify the longest flood of every year
find_longest_annual_floods <- function(flood_evt_data) {
  flood_evt_data %>% 
    group_by(site_no, year) %>% 
    # TODO: decide what to do about events that span a year. 
    #   can limit to each year by summarizing the prev fxn
    #   using evt id made up of year + flood?
    summarize(max_flood_evt_duration = max(flood_evt_length))
}
