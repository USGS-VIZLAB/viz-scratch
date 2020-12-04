
# Find minimum date in a daily dataset
get_min_date <- function(nwis_data) {
  min(nwis_data$Date)
}

add_gage_height <- function(nwis_data, rating_curve) {
  # Add GH (DEP = flow, INDEP = gage height)
  nwis_data$GH <- approx(rating_curve$DEP, rating_curve$INDEP, nwis_data$Flow)$y
  return(nwis_data)
}

# Using gage height and NWS flood stage, identify when a gage is flooding
identify_flooding <- function(gh_data, flood_info, flood_type = c("flood_stage", "moderate_flood_stage", "major_flood_stage")) {
  gh_data %>% 
    # TODO: how is this impacted by ICE?????
    filter(!is.na(GH)) %>% # Needs to have a GH 
    left_join(flood_info, by='site_no') %>%
    rename(flood_val = !!flood_type) %>% # Use the desired flood column
    mutate(is_flooding = GH >= as.numeric(flood_val)) %>%
    mutate(year = as.numeric(format(Date, "%Y"))) %>% 
    select(site_no, Date, year, GH, is_flooding)
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
