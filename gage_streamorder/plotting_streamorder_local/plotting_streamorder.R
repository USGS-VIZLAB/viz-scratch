
# load packages -------------------------------------------------------

library(tidyverse)
library(sf)
library(patchwork)
library(scales)
library(raincloudplots)
library(ggforce)
library(ggdist)
library(nhdplusTools)


theme_set(theme_classic(base_size = 14))

# Read and munge data for NE -----------------------------------------------------

## comids with gages
active_gages <- read_csv('USGS_WY20_sites.csv') # list of currently active gages from Chris Konrads (with comids)

## data pulled from nhd for all reaches in the NE
comid_nhd <- read_rds( "comid_nhd.rds") %>%
  bind_rows %>% 
  st_drop_geometry() %>% 
  filter(streamorde > 0) %>% 
  select(comid, streamorde)
str(comid_nhd)

## find streamorder for gaged reaches
comid_gaged <- comid_nhd %>% 
  filter(comid %in% active_gages$COMID) # NE reaches that are gaged
str(comid_gaged)

## combine datasets and calculate the proportion gaged in each streamorder
comid_all <- comid_gaged %>% 
  mutate(gage = "yes") %>%
  bind_rows(comid_nhd %>% mutate(gage = "no"))

comid_compare <- comid_all%>%
  group_by(gage, streamorde) %>%
  summarize(n = length(unique(comid))) %>%
  pivot_wider(names_from = gage, values_from = n) %>%
  mutate(perc = (yes/no)*100) # is this calculation of perc correct? shouldn't it be yes/(yes+no)*100?
comid_compare %>% str


## create new data structure for proportion plot
### try making it by mutating comid_compare
comids_prop <- comid_compare %>%
  mutate(perc_of_total_comids = 100*((comid_compare$no+comid_compare$yes)/sum(comid_compare$no+comid_compare$yes))) %>%
  mutate(perc_of_gaged_comids = 100*((comid_compare$yes)/sum(comid_compare$yes)))

### try making it another way, by deconstructing it and making a whole new dataframe
streamorder <- rep(c(1,2,3,4,5,6,7),2)
gaged <- c(rep("all",7), rep("gage",7))
percs <- c(comids_prop$perc_of_total_comids, comids_prop$perc_of_gaged_comids)
comids_prop_2 <- data.frame(gaged, streamorder, percs) ## this variable is the one being plotted for proportion plots now


## Add gaging data to geospatial flowlines
comid_geospatial <- read_rds( "comid_nhd.rds") %>%
  bind_rows %>%
  filter(streamorde > 0) %>%
  mutate(gage = ifelse(comid_geospatial$comid %in% comid_gaged$comid, "orange", "grey72"))

  

# Compose Plots  ---------------------------------------------

# histogram of stream order across all stream reaches
plot_gage <- comid_gaged %>%
  ggplot()+
  geom_histogram(aes(streamorde), binwidth = 1, alpha = .4)+ 
  scale_y_continuous(labels = scales::label_number(big.mark =','))+ # reformat y-axis labesl to have a comma
  labs(x = "Stream order", y = "Gaged reaches")+
  scale_x_continuous(breaks=1:7) # make sure all numbers shown on x-axis

# histogram of stream order for gaged reaches
plot_all <- comid_nhd %>%
  ggplot()+
  geom_histogram(aes(streamorde), binwidth = 1, alpha = .4)+
  scale_y_continuous(labels = scales::label_number(big.mark =','))+
  labs(x = "", y = "NE stream reaches")+
  scale_x_continuous(breaks=1:7)

## proportion of gaged streams in each order, lollipop style
plot_prop <- comid_compare %>%
  ggplot()+
  geom_segment(aes(x=streamorde, xend = streamorde, y=0, yend=perc), size = 1, alpha = 1, color = "grey")+
  geom_point(aes(streamorde, perc), size = 3, shape = 21, stroke = 1, fill="white")+
  theme(axis.text.y=element_blank(),axis.ticks.y=element_blank())+
  geom_text(aes(streamorde, perc+0.3, label = round(perc, 1)))+
  labs(x = "", y = "Percent gaged")+
  scale_x_continuous(breaks=1:7, limits=c(0.25,7.5))

## stacked histogram
plot_stacked_hist <- comid_all %>%
  ggplot()+
  geom_histogram(aes(x=streamorde, fill=gage), binwidth=1)+
  scale_y_continuous(labels = scales::label_number(big.mark =','))+ # reformat y-axis labels to have a comma
  labs(x = "Stream order", y = "Gaged reaches")+
  scale_x_continuous(breaks=1:7) # make sure all numbers shown on x-axis

plot_stacked_hist

## rainclouds
plot_raincloud <- ggplot(comid_all, aes(x=gage, y=streamorde)) +
  ggdist::stat_halfeye(
    adjust = .5,
    width = .6,
    .width = 0,
    justification = -.2,
    point_color = NA
  ) +
  geom_boxplot(
    width = .15,
    outlier.shape = NA
  ) +
  geom_point(
    shape = 95,
    size=10,
    alpha=0.4, 
  )


## stacked bar chart proportional comparison of streamorder of gaged comids vs order of all comids for proportion plot
plot_prop <- ggplot(
  comids_prop_2, 
  aes(fill=streamorder,
      y=percs,
      x=gaged)) + 
  geom_bar(
    position="stack", 
    stat="identity")+
  labs(x = "gaged", y = "percent of total comids")
  


# Plot everything -------------------------------------------------

## Histogram of all comids
plot_all

## Histogram of gaged comids
plot_gaged

## Map of stream reaches in New England, weighted by stream order, colored by whether it is gaged or not
plot(sf::st_geometry(comid_geospatial), 
     lwd = comid_geospatial$streamorde,
     col = comid_geospatial$gage
)

## Stacked proportional bar chart
plot_prop

## Raincloud plot of distribution
plot_raincloud

## Histogram of all gaged reaches by streamorder, with each bar actually as a stack color-coding whether or not it is gaged
plot_stacked_hist

## Two stacked proportional bar charts, comparing all reaches vs gaged reaches. Setup for proportion plot, but not implemented yet
plot_prop



