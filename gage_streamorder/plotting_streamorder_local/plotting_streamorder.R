
# plot stream order -------------------------------------------------------

library(tidyverse)
library(sf)
library(patchwork)
library(scales)

theme_set(theme_classic(base_size = 14))

# read in data for NE -----------------------------------------------------

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
comid_compare <- comid_gaged %>% 
  mutate(gage = "yes") %>%
  bind_rows(comid_nhd %>% mutate(gage = "no"))  %>%
  group_by(gage, streamorde) %>%
  summarize(n = length(unique(comid))) %>%
  pivot_wider(names_from = gage, values_from = n) %>%
  mutate(perc = (yes/no)*100) 
comid_compare %>% str

# 3-panel histogram lollipop  ---------------------------------------------

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

## stacked bar chart by count
plot_stacked_count <- comid_compare %>%
  ggplot() +
  geom_histogram(aes(streamord), binwidth=1, alpha-.4)+
  scale_y_continuous(labels = scales::label_number(big.mark = ',')) +
  scale_x-continuous(breaks=1:7)+
  labs(x = "Stream Order", y = "Gaged Reaches")

  
  geom_point(aes(streamorde, perc), size = 10, shape = 21, stroke = 1, fill="white")+
  theme(axis.text.y=element_blank(),axis.ticks.y=element_blank())+
  geom_text(aes(streamorde, perc+0.3, label = round(perc, 1)))+
  labs(x = "", y = "Percent gaged")+
  scale_x_continuous(breaks=1:7, limits=c(0.25,7.5))

plot_stacked_count

# plot them all together
plot_all + plot_gage + plot_prop


