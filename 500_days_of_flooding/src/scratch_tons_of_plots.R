# Code is too scratchy for even putting in the pipeline, but wanted to include this for posterity. 

library(dataRetrieval)
library(dplyr)
library(tidyr)
library(ggplot2)

site1 <- "06472000" # 545 days, Q since 1999, GH since 2017
site2 <- "06471000" # 513 days, Q since 1991, GH since 2018

sites_df <- readNWISsite(site1)

# Get and join NWS flood stage table
nws_flood_stage_list <- jsonlite::fromJSON("https://waterwatch.usgs.gov/webservices/floodstage?format=json")
nws_flood_stage_table <- nws_flood_stage_list[["sites"]]
sites_df <- sites_df %>%
  select(site_no, station_nm, dec_lat_va, dec_long_va) %>% 
  left_join(nws_flood_stage_table, by='site_no')


# Since we only have GH back to 2018-05-01 for site1,
# use rating curve to approximate Q for flood stage.
site_rating_curve <- readNWISrating(sites_df$site_no)
site_curve_gh <- site_rating_curve$INDEP
site_curve_q <- site_rating_curve$DEP
flood_q <- approx(site_curve_gh, site_curve_q, sites_df$flood_stage)$y
flood_q_mod <- approx(site_curve_gh, site_curve_q, sites_df$moderate_flood_stage)$y
flood_q_maj <- approx(site_curve_gh, site_curve_q, sites_df$major_flood_stage)$y

# DOWNLOAD FLOW
q_data <- readNWISdv(sites_df$site_no, parameterCd = "00060") %>% 
  renameNWISColumns() %>% 
  mutate(year = as.numeric(format(Date, "%Y")))

# COUNT FLOODING DAYS
count_consecutive_flood_days <- function(bool_col, grp) {
  unlist(lapply(split(bool_col, grp), cumsum))
}

q_data_flood <- q_data %>% 
  # For now treating all ice days as flood
  mutate(is_flooding = Flow >= flood_q | Flow_cd == "P Ice") %>% # Could be NA due to ice
  filter(year >= 1999) %>% 
  # Identify groups of consecutive flooding days
  mutate(grp = cumsum(c(FALSE, diff(is_flooding) < 0))) %>% 
  # Count consecutive flooding days
  mutate(cumulative_consecutive_flood = count_consecutive_flood_days(is_flooding, grp)) %>% 
  group_by(grp) %>% 
  mutate(grp_max = max(cumulative_consecutive_flood),
         cum_q = cumsum(Flow))

##### Sailboat plots #####
q_data_flood %>% 
  ggplot(aes(x = Date, y = cumulative_consecutive_flood)) +
  geom_line() +
  # geom_curve(aes(xend = lag(Date), yend = lag(cumulative_consecutive_flood))) +
  ggtitle(sites_df$site_no) +
  ggtitle("TEST 1: Compare flood events through time based on duration & magnitude")

# Make peaks look like waves
q_data_flood %>% 
  ggplot(aes(x = Date, y = cumulative_consecutive_flood), fill = grp_max) +
  geom_polygon() + 
  scale_color_gradient2(low = "lightblue", high = "darkblue") +
  # geom_curve(aes(xend = lag(Date), yend = lag(cumulative_consecutive_flood))) +
  ggtitle(sites_df$site_no)+
  ggtitle("TEST 2: Compare flood events through time based on duration & magnitude")


###### Bar plot: width = span, height = max number of consecutive days #####

# Try to get magnitude + duration

# Bar height = max flow of event
# Bar width = max # consecutive days of event
# Bar position = median date of event

q_data_flood %>% 
  filter(cumulative_consecutive_flood > 0) %>% 
  group_by(grp) %>% 
  summarize(bar_height = max(Flow),
            bar_width = max(cumulative_consecutive_flood),
            bar_x_loc = median(Date)) %>%
  mutate(bar_x1 = bar_x_loc - bar_width/2,
         bar_x2 = bar_x_loc + bar_width/2) %>% 
  ggplot(aes(xmin = bar_x1, xmax = bar_x2, ymax = bar_height)) +
  geom_rect(ymin = 0) + 
  geom_hline(yintercept = flood_q, color = "orange", linetype = "dashed", size = 1) +
  geom_hline(yintercept = flood_q_mod, color = "red", linetype = "dashed", size = 1) +
  geom_hline(yintercept = flood_q_maj, color = "maroon", linetype = "dashed", size = 1) + 
  ylab("Peak Event Flow") + xlab("Days") +
  ggtitle("TEST 3: Compare flood events through time based on duration & magnitude")


##### Scatter plot style #####

q_data_flood %>% 
  filter(cumulative_consecutive_flood > 0) %>% 
  group_by(grp) %>% 
  summarize(x = median(Date),
            y = max(cumulative_consecutive_flood),
            size = max(Flow)) %>% 
  ggplot(aes(x = x, y = y, size = size)) +
  geom_point() +
  ylab("Days of flooding") + xlab("Year") +
  ggtitle("TEST 4: Compare flood events through time based on duration & magnitude")

###### Heat Maps #####

# Filled based on flow magnitude
q_data_flood %>% 
  mutate(doy = as.numeric(format(Date, "%j"))) %>% 
  ggplot(aes(x = doy, y = year, fill = Flow)) +
  geom_tile() +
  theme_minimal() +
  theme(panel.grid = element_blank()) +
  scale_fill_gradient2(low="#DCEDC8", mid = "#42B3D5", high="#1A237E", midpoint = 5000) +
  ggtitle("TEST 5: Heat map to show flow magnitude through time")

# Filled based on binary idea of below or above flood
q_data_flood %>% 
  mutate(doy = as.numeric(format(Date, "%j"))) %>% 
  ggplot(aes(x = doy, y = year, fill = is_flooding)) +
  geom_tile() +
  scale_fill_manual(values = c("lightgrey", "navy")) +
  theme_minimal() +
  theme(panel.grid = element_blank()) +
  ggtitle("TEST 6: Heat map to show flood event occurrence")

# Filled based on type of flood
q_data_flood_levels <- q_data %>% 
  # For now treating all ice days as flood
  mutate(flood_type = 
           ifelse(is.na(Flow), #Flow_cd == "P ice" | Flow_cd == "P Ice i" | 
                  "UNKNOWN", 
                  ifelse(Flow >= flood_q_maj,
                         "Major flood",
                         ifelse(Flow >= flood_q_mod,
                                "Moderate flood",
                                ifelse(Flow >= flood_q,
                                       "Minor flood",
                                       "No flood"))))
  ) %>% # Could be NA due to ice
  mutate(flood_type = factor(flood_type, levels = c("Major flood", "Moderate flood", "Minor flood", "No flood", "UNKOWN"))) %>% 
  filter(year >= 1999) %>% 
  mutate(doy = as.numeric(format(Date, "%j"))) 

q_data_flood_levels %>% 
  ggplot(aes(x = doy, y = year, fill = flood_type)) +
  geom_tile() +
  viridis::scale_fill_viridis(discrete = TRUE) +
  theme_minimal() +
  theme(panel.grid = element_blank()) +
  ggtitle("TEST 7: Heat map to show difference in magnitude and duration flood events across time")

##### Try a plot where we compare other sites of similar specs #####

min_date_to_use <- min(q_data_flood$Date)
similar_sites <- purrr::map(sprintf("%02d", 1:21), function(huc, drain_area) {
  if(huc %in% c("04", "06", "20", "21")) return() # These HUCs failed bc of no data to return
  whatNWISsites(huc = huc, parameterCd = c("00065"), startDate = min_date_to_use,
    drainAreaMin = drain_area - drain_area*0.25, drainAreaMax = drain_area + drain_area*0.25) %>% 
    select(site_no, station_nm, dec_lat_va, dec_long_va)
}, drain_area = readNWISsite(site1)$drain_area_va) %>% bind_rows()

other_sites_df <- similar_sites %>%
  filter(site_no != site1) %>%  
  left_join(nws_flood_stage_table, by='site_no') %>% 
  filter(!is.na(flood_stage)) # Must have flood stage
n_other_sites <- length(unique(other_sites_df$site_no))

q_data_others <- readNWISdv(other_sites_df$site_no, parameterCd = "00065", startDate = min_date_to_use) %>% 
  renameNWISColumns() %>% 
  mutate(year = as.numeric(format(Date, "%Y"))) %>% 
  left_join(other_sites_df, by = "site_no") %>% 
  mutate(is_flooding = GH >= as.numeric(flood_stage)) %>% 
  select(site_no, Date, GH, year, is_flooding)

q_data_flood_others <- q_data_others %>% 
  filter(!is.na(GH)) %>%
  group_by(site_no) %>%
  # Identify groups of consecutive flooding days
  mutate(grp = sprintf("%s_%s", site_no, cumsum(c(FALSE, diff(is_flooding) < 0)))) %>% 
  # Count consecutive flooding days
  mutate(cumulative_consecutive_flood = count_consecutive_flood_days2(is_flooding, grp)) %>% 
  group_by(site_no, grp) %>% 
  mutate(grp_max = max(cumulative_consecutive_flood)) %>% 
  ungroup()

# Make a plot where the filled in polys are this site and the unfilled are
#   the other sites
ggplot() +
  # geom_line(data = q_data_flood_others, aes(x = Date, y = cumulative_consecutive_flood, color = site_no)) +
  geom_curve(data = q_data_flood, aes(x = Date, xend = lead(Date), y = cumulative_consecutive_flood, 
                                      yend = lead(cumulative_consecutive_flood)), color = "#3f5f5c80", size=2) +
  scale_color_manual(values = rep("#9f817f80", n_other_sites)) + # Make the background lines all the same color
  # geom_hline(yintercept = max(q_data_flood_others$cumulative_consecutive_flood), color = "#5e696f", linetype = "dashed", size=0.8) +
  # geom_text(
  #   aes(x = min_date_to_use, y = max(q_data_flood_others$cumulative_consecutive_flood)),
  #   label = sprintf("Maximum duration of consecutive flood\ndays from %s other similar sites", n_other_sites),
  #   nudge_y = 30, hjust = 0) +
  geom_text(aes(x = min_date_to_use, y = 500), label = "Site of interest", color = "#3f5f5c", size=8, fontface = "bold", hjust = 0) +
  # geom_text(aes(x = min_date_to_use, y = 450), label = "Other sites to compare", color = "#9f817f", size=8, fontface = "bold", hjust = 0) +
  guides(color = FALSE) +
  theme_bw() +
  ylab("Cumulative consecutive flood days") + xlab("Date") +
  ggtitle("TEST 8: Compare flood events from other sites over time based on duration")

##### A line + scatter chart hybrid #####

# Inspired by https://www.nytimes.com/interactive/2017/09/01/upshot/cost-of-hurricane-harvey-only-one-storm-comes-close.html

# Radius = consecutive days
# Use data pulled from above to compare other similar sites to this one

q_data_flood_arcs <- q_data_flood %>% 
  filter(cumulative_consecutive_flood > 0) %>% 
  group_by(grp) %>% 
  summarize(bar_color = max(Flow),
            arc_size = max(cumulative_consecutive_flood),
            arc_x_loc = median(Date)) %>%
  mutate(arc_x_loc_day = as.numeric(format(arc_x_loc, "%j")),
         year = as.numeric(format(arc_x_loc, "%Y")))

q_data_flood_arcs_others <- q_data_flood_others %>% 
  filter(cumulative_consecutive_flood > 0) %>% 
  group_by(site_no, grp) %>% 
  summarize(bar_color = max(GH), # IF YOU USE THIS YOU ARE COMPARING GH TO FLOW (site1 only has q)
            arc_size = max(cumulative_consecutive_flood),
            arc_x_loc = median(Date)) %>%
  mutate(arc_x_loc_day = as.numeric(format(arc_x_loc, "%j")),
         year = as.numeric(format(arc_x_loc, "%Y")))

ggplot() +
  # All "other" sites
  ggforce::geom_arc_bar(
    data = q_data_flood_arcs_others, fill = "#9f817f80", color = NA,
    aes(x0 = arc_x_loc_day, y0 = 0, r0 = 0, r = sqrt(arc_size), start = -pi/2, end = pi/2)) +
  # This record-breaking site
  ggforce::geom_arc_bar(
    data = q_data_flood_arcs, fill = "#3f5f5c80", color = "#5e696f", size = 1,
    aes(x0 = arc_x_loc_day, y0 = 0, r0 = 0, r = sqrt(arc_size), start = -pi/2, end = pi/2)) +
  coord_fixed() +
  ylab("Days of consecutive flooding") + xlab("Day of Year") +
  geom_text(data=tibble(x = 0, y = 20, year = 1999), aes(x = x, y = y),
            label = "Site of interest", color = "#3f5f5c", size=4.5, fontface = "bold", hjust = 0) +
  geom_text(data=tibble(x = 0, y = 7, year = 1999), aes(x = x, y = y),
            label = "Other sites to compare", color = "#9f817f", size=4.5, fontface = "bold", hjust = 0) +
  facet_grid(rows = vars(year)) +
  theme(panel.background = element_blank(), 
        strip.background = element_blank(),
        strip.text.y = element_text(angle = 0),
        axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(),
        plot.margin = unit(c(0, 0, 0, 0), "null")) +
    ggtitle("TEST 9: Compare flood events from other sites over time based on duration using circles!")

# Do a rectangle version of the above
# similiar to: https://flowingdata.com/2020/09/10/a-timeline-of-california-wildfires/

# Split grps if they span a year (they will still get the max value for peak consecutive flood days)
q_data_flood_span_yrs <- q_data_flood %>% 
  mutate(grp_new = sprintf("%s_%s", year, grp)) %>% 
  filter(cumulative_consecutive_flood > 0) %>% 
  group_by(grp_new) %>% 
  summarize(site_no = unique(site_no),
            year = unique(year),
            peak_consecutive_flood_days = unique(grp_max),
            start_Dt = min(Date),
            end_Dt = max(Date),
            x_start = as.numeric(format(min(Date), "%j")),
            x_end = as.numeric(format(max(Date), "%j")))
  
q_data_flood_span_yrs_others <- q_data_flood_others %>% 
  mutate(grp_new = sprintf("%s_%s", year, grp)) %>% 
  filter(cumulative_consecutive_flood > 0) %>% 
  group_by(grp_new) %>% 
  summarize(site_no = unique(site_no),
            year = unique(year),
            peak_consecutive_flood_days = unique(grp_max),
            start_Dt = min(Date),
            end_Dt = max(Date),
            x_start = as.numeric(format(min(Date), "%j")),
            x_end = as.numeric(format(max(Date), "%j")))

ggplot() +
  # All "other" sites
  geom_rect(
    data = q_data_flood_span_yrs_others, fill = "#9f817f80", color = NA,
    aes(xmin = x_start, xmax = x_end, ymin = 0, ymax = peak_consecutive_flood_days)) +
  # This record-breaking site
  geom_rect(
    data = q_data_flood_span_yrs, fill = "#3f5f5c80", color = "#5e696f", size = 1,
    aes(xmin = x_start, xmax = x_end, ymin = 0, ymax = peak_consecutive_flood_days)) +
  geom_text(data=tibble(x = 0, y = 400, year = 1999), aes(x = x, y = y),
            label = "Site of interest", color = "#3f5f5c", size=4.5, fontface = "bold", hjust = 0) +
  geom_text(data=tibble(x = 0, y = 150, year = 1999), aes(x = x, y = y),
            label = "Other sites to compare", color = "#9f817f", size=4.5, fontface = "bold", hjust = 0) +
  facet_grid(rows = vars(year)) +
  ylab("N days of consecutive flooding") + xlab("Day of Year") +
  theme(panel.background = element_blank(), 
        strip.background = element_blank(),
        strip.text.y = element_text(angle = 0),
        axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(),
        plot.margin = unit(c(0, 0, 0, 0), "null")) +
  ggtitle("TEST 10A: Compare flood events from other sites over time based on duration using rectangles")

ggplot() +
  # All "other" sites
  geom_rect(
    data = q_data_flood_span_yrs_others, fill = "#9f817f80", color = NA,
    aes(xmin = start_Dt, xmax = end_Dt, ymin = 0, ymax = peak_consecutive_flood_days)) +
  # This record-breaking site
  geom_rect(
    data = q_data_flood_span_yrs, fill = "#3f5f5c80", color = "#5e696f", size = 1,
    aes(xmin = start_Dt, xmax = end_Dt, ymin = 0, ymax = peak_consecutive_flood_days)) +
  facet_grid(rows = vars(site_no)) +
  ylab("N days of consecutive flooding") + xlab("Day of Year") +
  theme(panel.background = element_blank(), 
        strip.background = element_blank(),
        strip.text.y = element_text(angle = 0)) +
  ggtitle("TEST 10B: Compare flood events from other sites over time based on duration using rectangles, keep timeline, grp by site")


# Curved version with fixed year span issues
q_data_flood_span_yrs_arc_others <- q_data_flood_span_yrs_others %>% 
  # mutate(curvature = approx(c(0,365), c(-0.7, -0.05), peak_consecutive_flood_days)$y) %>% 
  filter(x_start != x_end)

ggplot() +
  # All "other" sites
  geom_curve(
    data = q_data_flood_span_yrs_arc_others, 
    aes(x = x_start, xend = x_end, y = year*100, yend = year*100), curvature = -0.25, color = "#9f817f80") +
  # This record-breaking site
  geom_curve(
    data = q_data_flood_span_yrs, color = "#5e696f", size = 1,
    aes(x = x_start, xend = x_end, y = year*100, yend = year*100), curvature = -0.25) +
  ylim(min(q_data_flood_span_yrs$year)*100, (max(q_data_flood_span_yrs$year)+2)*100) +
  ylab("N days of consecutive flooding") + xlab("Day of Year") +
  geom_text(data=tibble(x = 0, y = 1999.5*100, year = 1999), aes(x = x, y = y),
            label = "Site of interest", color = "#3f5f5c", size=4.5, fontface = "bold", hjust = 0) +
  geom_text(data=tibble(x = 0, y = 1999*100, year = 1999), aes(x = x, y = y),
            label = "Other sites to compare", color = "#9f817f", size=4.5, fontface = "bold", hjust = 0) +
  theme(panel.background = element_blank(), 
        strip.background = element_blank(),
        strip.text.y = element_text(angle = 0),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        plot.margin = unit(c(0, 0, 0, 0), "null")) +
  ggtitle("TEST 11: Compare flood events from other sites over time based on duration using arcs")


##### Line chart inspired by NYT Covid chart #####
# https://www.nytimes.com/interactive/2020/11/25/us/coronavirus-cases-rising.html?action=click&module=Top%20Stories&pgtype=Homepage

q_data_flood_seg <- q_data_flood %>% 
  group_by(year) %>% 
  summarize(peak_consecutive_flood_days = max(cumulative_consecutive_flood))
q_data_flood_seg_others <- q_data_flood_others %>% 
  group_by(year, site_no) %>% 
  summarize(peak_consecutive_flood_days = max(cumulative_consecutive_flood))

ggplot() +
  # "OTHER" sites
    # Sloped lines for other sites
    geom_segment(data = q_data_flood_seg_others,
                 aes(x = year - 1, xend = year, y = 0, yend = peak_consecutive_flood_days),
                 size = 0.8, color = "#9f817f4D") +
    # Straight lines connecting sloped to x-axis for other sites
    geom_segment(data = q_data_flood_seg_others,
                 aes(x = year, xend = year, y = 0, yend = peak_consecutive_flood_days),
                 size = 0.8, color = "#d8cccb4D", linetype = "dotted") +
  
  # SITE 1
    # Sloped lines for just site1
    geom_segment(data = q_data_flood_seg, 
                 aes(x = year - 1, xend = year, y = 0, yend = peak_consecutive_flood_days), 
                 size = 0.8, color = "#3f5f5cCC") +
    # Straight lines connecting sloped to x-axis for just site1
    geom_segment(data = q_data_flood_seg,
                 aes(x = year, xend = year, y = 0, yend = peak_consecutive_flood_days), 
                 size = 0.8, color = "#b2bfbdCC", linetype = "dotted") +
    # Dots for just site 1
    geom_point(data = q_data_flood_seg, aes(x = year - 1, y = 0), color = "#3f5f5c") +
    geom_point(data = q_data_flood_seg, aes(x = year, y = peak_consecutive_flood_days), color = "#3f5f5c") +
  
  # Styling
  theme(panel.background = element_blank(),
        axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5)) +
  ylab("Peak consecutive days of flooding") +
  xlab("Year") +
  ggtitle("TEST 12: Compare peak consecutive days of flood for events by year from other sites over time")

# Try a version of the segment approach where you don't summarize by years
# and keep as flood events instead. Things on the x-axis won't line up
# neatly, so it may be crazy to look at.
q_data_flood_seg_evt <- q_data_flood %>% 
  filter(cumulative_consecutive_flood > 0) %>% 
  mutate(GH = approx(site_curve_q, site_curve_gh, Flow)$y) %>% 
  group_by(grp) %>% 
  summarize(peak_gh = max(GH, na.rm = TRUE),
            peak_consecutive_flood_days = max(cumulative_consecutive_flood),
            evt_start = min(Date),
            evt_end = max(Date))
q_data_flood_seg_evt_others <- q_data_flood_others %>% 
  filter(cumulative_consecutive_flood > 0) %>% 
  group_by(grp, site_no) %>% 
  summarize(peak_gh = max(GH, na.rm = TRUE),
            peak_consecutive_flood_days = max(cumulative_consecutive_flood),
            evt_start = min(Date),
            evt_end = max(Date))

ggplot() +
  # "OTHER" sites
  # Sloped lines for other sites
  geom_segment(data = q_data_flood_seg_evt_others,
               aes(x = evt_start, xend = evt_end, y = 0, yend = peak_gh),
               size = 0.8, color = "#9f817f4D") +
  # Straight lines connecting sloped to x-axis for other sites
  geom_segment(data = q_data_flood_seg_evt_others,
               aes(x = evt_end, xend = evt_end, y = 0, yend = peak_gh),
               size = 0.8, color = "#d8cccb4D", linetype = "dotted") +

  # SITE 1
  # Sloped lines for just site1
  geom_segment(data = q_data_flood_seg_evt, 
               aes(x = evt_start, xend = evt_end, y = 0, yend = peak_gh), 
               size = 0.8, color = "#3f5f5cCC") +
  # Straight lines connecting sloped to x-axis for just site1
  geom_segment(data = q_data_flood_seg_evt,
               aes(x = evt_end, xend = evt_end, y = 0, yend = peak_gh), 
               size = 0.8, color = "#b2bfbdCC", linetype = "dotted") +
  # Dots for just site 1
  geom_point(data = q_data_flood_seg_evt, aes(x = evt_start, y = 0), color = "#3f5f5c") +
  geom_point(data = q_data_flood_seg_evt, aes(x = evt_end, y = peak_gh), color = "#3f5f5c") +
  scale_y_log10() + 
  # Styling
  theme(panel.background = element_blank(),
        axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5)) +
  ylab("Peak consecutive days of flooding") +
  xlab("Date") +
  ggtitle("TEST 13: Compare peak consecutive days of flood for events from other sites over time")

