# histogram of gages by stream order or drainage area ---------------------
library(dataRetrieval)
library(ggplot2)
library(tidyverse)


# for all gages (as of 2019) - thesite_no, number of years active, when started, and any inactive years
gages <- readRDS("data/active_flow_gages_summary_wy.rds")
str(gages)

# make a list of all gages
gage_list <- gages %>% pull(site) %>% unique

## find drainage area from NWIS
readNWISsite(gage_list[1]) # for a single site
gage_da <- readNWISsite(gage_list) # for a all sites

# what is the range of draingage and contributing drainage areas?
range(na.omit(gage_da$drain_area_va))
range(na.omit(gage_da$contrib_drain_area_va))

# plot a histogram using drainage area
gage_da %>%
  ggplot(aes(drain_area_va)) +
  geom_histogram(binwidth = .1) +
  scale_x_log10()


