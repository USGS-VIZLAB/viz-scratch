## ACTIVE GAGE RECORDS

library(tidyverse)
library(patchwork)
library(gghighlight)
library(beeswarm)
library(ggdist)
library(gghalves)

# functions ---------------------------------------------------------------

substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

## gages through the ages data ---------------------------------------------

# for all gages (as of 2019) - thesite_no, number of years active, when started, and any inactive years
gages <- readRDS("data/active_flow_gages_summary_wy.rds")
str(gages)

# duration of most recent active period -----------------------------------

# create a list of gages that are "currently" active (in 2019)
gage_active <- gages %>% 
  unnest(which_years_active) %>%
  filter(which_years_active == 2019) %>%
  pull(site) %>%
  unique()
gage_active  # some gages have gaps in activity

## separate gages with and without gaps, to calculate the duration of the most recent period of activity for each one

# gages with no gaps
gages_continuous_current <- gages %>% 
  filter(!any_gaps) %>% 
  rowwise() %>% 
  mutate(is_current = 2019 %in% unlist(which_years_active)) %>% 
  ungroup() %>% 
  filter(is_current) %>% 
  mutate(gaps = 'no') %>%
  select(site, n_years_active, earliest_active_year, gaps)

# gages with gaps
gages_continuous_current_gaps <- active_flow_gages_summary %>% 
  filter(any_gaps) %>% 
  rowwise() %>% 
  mutate(is_current = 2019 %in% unlist(which_years_active)) %>% 
  ungroup() %>% 
  filter(is_current) %>%
  rowwise() %>%
  mutate(last_gap = as.numeric(substrRight(max(unlist(gap_years)), 4))) %>%
  ungroup() %>% 
  mutate(n_years_active = 2019 - last_gap, gaps = 'yes',
         earliest_active_year = last_gap)%>% 
  select(site, n_years_active, earliest_active_year, gaps)

# combine gaps and no gap data
records <- rbind(gages_continuous_current_gaps, gages_continuous_current)


# plot period of record ---------------------------------------------------

# plot distribution
records %>%
  ggplot(aes(earliest_active_year, fill = gaps))+
  geom_histogram(binwidth = 1) +
  gghighlight() + 
  facet_wrap(~gaps) +
  theme_minimal() 

## make raincloud plots

# earliest active year of continuous data
ggplot(records%>%mutate(same = ' '), aes(x = same, earliest_active_year))+
  ggdist::stat_halfeye(adjust = .5, width = .6, 
    justification = -.2, .width = 0, point_colour = NA)+
  geom_boxplot(
    width = .12, outlier.color = NA ) +
  ggdist::stat_dots(
    side = "left", justification = 1.1,  binwidth = .25) + 
  coord_cartesian(xlim = c(1.2, NA))+
  theme_classic()+
  coord_flip()+
  labs(y="Earliest active year of continuous data",x="")+
  theme(axis.line.y=element_blank())+
  geom_hline(yintercept = median(records$earliest_active_year), color="darkorchid", size=1.5, alpha=.3)+
  annotate("text", y= median(records$earliest_active_year)-6, x=1.5, label= median(records$earliest_active_year), color="darkorchid")

# duration of current continuous record
ggplot(records%>%mutate(same = ' '), aes(x = same, n_years_active))+
  ## add half-violin from {ggdist} package
  ggdist::stat_halfeye(
    ## custom bandwidth
    adjust = .5, 
    ## adjust height
    width = .6, 
    ## move geom to the right
    justification = -.2, 
    ## remove slab interval
    .width = 0, 
    point_colour = NA)+
  geom_boxplot(
    width = .12, 
    ## remove outliers
    outlier.color = NA ## `outlier.shape = NA` works as well
  ) +
  ## add dot plots from {ggdist} package
  ggdist::stat_dots(
    ## orientation to the left
    side = "left", 
    ## move geom to the left
    justification = 1.1, 
    ## adjust grouping (binning) of observations 
    binwidth = .25
  ) + 
  ## remove white space on the left
  coord_cartesian(xlim = c(1.2, NA))+
  theme_classic(base_size=14)+
  coord_flip()+
  labs(y="Duration of current continuous record (years)",
       x="")+
  theme(axis.line.y=element_blank())+
  geom_hline(yintercept = median(records$n_years_active), color="orangered", size=1.5, alpha=.3)+
  annotate("text", y= median(records$n_years_active)+2, x=1.5, label= median(records$n_years_active), color="orangered")

