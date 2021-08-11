# area chart cartogram ----------------------------------------------------

library(tidyverse)
library(lubridate)
library(dataRetrieval)
library(geofacet)
library(maps)
library(patchwork)

# streamflow data ---------------------------------------------------------

## dates for geofacet map
date_start <- as.Date("2021-07-01")
date_end <- as.Date("2021-08-01")

## entire last year of River Conditions 
dv <- readRDS("/Users/cnell/Documents/Projects/gage-conditions-gif/2_process/out/dv_stats.rds") %>% 
  mutate(date = as.Date(dateTime),
         cond = 
           case_when(
             per < 0.05 ~ 'Driest',
             per >= 0.05 & per < 0.1 ~ "Drier",
             per >= 0.1 & per < 0.25 ~ "Dry", 
             per >= 0.25 & per < 0.75 ~ "Normal",
             per >= 0.75 & per < 0.9 ~ "Wet", 
             per >= 0.9 ~ "Wettest"
           ),
         cond = factor(cond, ordered = TRUE, levels = c("Driest", "Drier", "Dry", "Normal","Wet","Wettest"))) %>%
  filter(date >= date_end - 365, !is.na(cond))

## pull in site info to get locations by state
dv_site <- readNWISsite(siteNumbers = unique(dv$site_no)) %>%
  distinct(site_no, state_cd)

# proportion of gages in each flow category -------------------------------
## calculating at national and state levels

## total number of streamgages active each day by state
sites_state <- dv %>%
  left_join(dv_site)%>%
  group_by(state_cd, date) %>%
  summarize(total_gage = length(unique(site_no)))  %>% 
  mutate(fips = as.numeric(state_cd))  # for binding to cartogram grid

## nationally
sites_national <- sites_state %>%
  group_by(date) %>%
  summarize(total_gage = sum(total_gage))

## national timeseries data by flow level
yr_national <- dv %>%
  group_by(date, cond) %>%
  summarize(n_gage = length(unique(site_no))) %>%
  left_join(sites_national) %>%
  mutate(prop = n_gage/total_gage)

## national data 
july_national <- yr_national %>%
  filter(date >= date_start, date <= date_end) 

# state level - july only
july_states <- dv  %>%
  filter(date >= date_start, date <= date_end) %>%
  left_join(dv_site) %>% # adds state info for each gage
  group_by(state_cd, date, cond) %>% # aggregate by state, day, flow condition
  summarize(n_gage = length(unique(site_no)))  %>% 
  left_join(sites_state) %>% # add total_gage
  mutate(prop = n_gage/total_gage) %>% # proportion of gages
  pivot_wider(id_cols = !n_gage, names_from = cond, values_from = prop, values_fill = 0) %>% # complete data for timepoints with 0 gages
  pivot_longer(cols = Dry:Drier, names_to = "cond", values_to = "prop")

# area chart cartogram ----------------------------------------------------

# wet to dry color scale
pal_wetdry <- c("#002D5E", "#0C7182", "#A7D2D8", "#DDDD8C", "#AF9423", "#A84E0B")
color_bknd <- "#F4F4F4"

# plotting theme
theme_flowfacet <- function(base = 14) {
  theme_classic(base_size = base) +
    theme(strip.background = element_blank(),
          strip.text = element_text(size = 12, vjust = 1),
          strip.placement = "inside",
          strip.background.x = element_blank(),
          axis.line = element_blank(),
          axis.ticks = element_blank(),
          axis.text = element_blank(),
          axis.title = element_blank(),
          panel.border = element_blank(),
          plot.title = element_text(size = 18, face = "bold"),
          plot.background = element_rect(fill = color_bknd,
                                         color = color_bknd),
          panel.background = element_rect(fill = color_bknd,
                                          color = color_bknd),
          panel.spacing = unit(0, "pt"),
          legend.position = "none",
          plot.margin = margin(0, 50, 0, 50, "pt"))
}


## create custom grid to includes Puerto Rico
usa_grid <- us_state_grid1 %>% 
  add_row(row = 7, col = 11, code = "PR", name = "Puerto Rico") %>% # add PR
  filter(code != "DC") # remove DC (only has 3 gages)

## state fips code for grid; include islands
fips <- state.fips %>% 
  distinct(fips, abb) %>%
  add_row(fips = 02, abb = 'AK')%>%
  add_row(fips = 15, abb = 'HI')%>%
  add_row(fips = 72, abb = 'PR') %>%
  mutate(state_cd = str_pad(fips, 2, "left", pad = "0"))

## area chart cartogram
plot_cart <- july_states %>% 
  left_join(fips) %>% # to bind to cartogram grid
  ggplot(aes(date, prop)) +
  geom_area(aes(fill = cond)) +
  scale_fill_manual(values = rev(pal_wetdry)) +
  facet_geo(~abb, grid = usa_grid, move_axes = FALSE) +
  scale_y_continuous(trans = "reverse") +
  theme_flowfacet() +
  theme(plot.margin = margin(80, 50, 80, 50, "pt"),
        panel.spacing.y = unit(0, "pt"),
        strip.text = element_text(vjust = -1))+
  coord_fixed(ratio = 28)

# national level chart doubling as a legend
## breaks for second y axis with flow level labels
sec_labels <- july_national  %>%
  filter(date == max(july_national$date)) %>%
  distinct(cond, prop) %>%
  mutate(prop = cumsum(prop))

plot_nat <- july_national %>% 
  ggplot(aes(date, prop)) +
  geom_area(aes(fill = cond))+
  theme_classic() +
  labs(x=month(date_end - 30, label = TRUE, abbr = FALSE),
       y="") +
  scale_fill_manual(values = rev(pal_wetdry)) +
  scale_y_continuous(trans = "reverse",
                     breaks = rev(c(0.05,0.08, 0.5, 0.75, 0.92, 0.95)), 
                     labels = c("0%","","","","", "100%"),
                     sec.axis = dup_axis(
                       labels = sec_labels$cond
                     )) +
  theme_flowfacet() +
  theme(axis.text.y = element_text(size = 14, 
                                   vjust = c(1, 0), # align yaxis labels to plot bounds
                                   hjust = 1),
        axis.title.x.bottom = element_text(size = 26,
                                           vjust = 2),
        axis.title.x.top = element_text(size = 26,
                                        vjust = -1),
        axis.text.x.bottom = element_text(size = 14,
                                          vjust = 4),
        plot.margin = margin(0, 0, 0, 0, "pt")) +
  scale_x_date(breaks = seq.Date(date_start, date_end, "1 week"),
               position = "bottom",
               labels = day(seq.Date(date_start, date_end, "1 week")),
               sec.axis = dup_axis(
                 name = "National"
               )) +
  guides(fill = guide_legend("")) +
  coord_fixed(ratio = 28)

# piece together ----------------------------------------------------------

plot_blank <- ggplot()+
  geom_blank()+ 
  theme(panel.background = element_rect(fill = "blue",
                                        color = color_bknd),
        plot.background = element_rect(fill = color_bknd,
                                       color = color_bknd))

((plot_blank / plot_nat / plot_blank) | plot_cart) + 
  plot_layout(widths = c(1, 4),
              heights = c(.5, 3, .5, 4)) &
  theme(panel.background = element_rect(fill = color_bknd,
                                        color = color_bknd),
        plot.background = element_rect(fill = color_bknd,
                                       color = color_bknd))


ggsave(sprintf('out/flow_cartogram_%s_%s.svg', year(date_end), month(date_end - 30, label = TRUE)), width = 19, height = 9)

