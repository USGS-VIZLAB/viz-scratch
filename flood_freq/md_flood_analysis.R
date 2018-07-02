# This is code developed to produce a plot that is
# used to get an idea if a particular flood event was
# somehow very unusual. It uses the peak flow NWIS service
# to get historical peak flows. 
# The graph itself is a log-log plot of drainage area vs. 
# (peak flow / drainage area). 
# In this particular example, there were also 3 peaks measured 
# during 2 recent flash floods. These peaks were not at historical
# USGS sites, therefore they don't have information in the
# peak service. 
# We also added a "map legend" to get a general idea where the
# dots spanned geographically, and scaled the size and alpha to
# get a general idea how long ago the peaks were observed
# and how developed the land use was. We wanted to see
# if it looked like the peak flows were changing and if it 
# had to do with a changing landscape (neither seemed to be the case).
# We also added flood frequency information from the
# StreamStat service (ie... the "100yr flood" line)


library(dataRetrieval)
library(dplyr)
library(tidyr)
library(ggplot2)

md_sites <- whatNWISdata(stateCd = "MD", parameterCd = "00060")

md_sites <- md_sites %>%
  mutate(years = as.numeric(difftime(end_date, begin_date, units = "weeks"))/52.25) %>%
  arrange(desc(years)) %>%
  filter(data_type_cd == "dv" &
         stat_cd == "00003")

md_peaks <- readNWISpeak(unique(md_sites$site_no))
md_sites_expand <- readNWISsite(unique(md_sites$site_no))

flood_dates <- c("2016-07-30","2018-05-27")

graph_data <- md_peaks %>%
  left_join(select(md_sites_expand, site_no, drain_area_va), by="site_no") %>%
  mutate(discharge_perDA = peak_va/drain_area_va) %>%
  select(site_no, discharge_perDA, drain_area_va, peak_va, peak_dt) %>%
  group_by(site_no) %>%
  filter(discharge_perDA == max(discharge_perDA, na.rm = TRUE)) %>%
  mutate(fromServices = TRUE)

# Tiber:  DA=0.53 sq mi; computed peak=2,100 cfs
# Hudson: DA=1.26 sq mi; computed peak=2,750 cfs
# New Cut: DA=1.53 sq mi; computed peak=3,320 cfs

site_no = c("Tiber","Hudson", "New Cut")
drain_area_va = c(0.53, 1.26, 1.53)
peak_va = c(2100,2750,3320)
peak_dt = rep(Sys.Date(),3)
discharge_perDA = peak_va/drain_area_va
yr100 <- c(842, 1490, 1630)
yr200 <- c(1160, 1970, 2200)
yr500 <- c(1750, 2800, 3200)

new_data <- data.frame(site_no = site_no,
                       drain_area_va = drain_area_va,
                       peak_va = peak_va,
                       discharge_perDA = discharge_perDA,
                       peak_dt = peak_dt,
                       yr100 = yr100,
                       yr200 = yr200,
                       yr500 = yr500,
                       fromServices = FALSE,stringsAsFactors = FALSE)

graph_data <- bind_rows(graph_data, new_data)

graph_data$peak_dt[is.na(graph_data$peak_dt)] <- as.Date("1900-01-01")

graph_1 <- ggplot(data = graph_data) +
  geom_point( aes(x=drain_area_va, y=discharge_perDA, color = fromServices, alpha = peak_dt), size = 3) +
  scale_y_log10(limits = c(1,10000)) +
  scale_x_log10(limits = c(.1,100000)) +
  scale_color_manual(values = c("red","black")) +
  theme(legend.position = "none") +
  xlab("Drainage area in square miles") +
  ylab("Discharge in cubic feet per second per square mile") +
  ggtitle("Full period of record", subtitle = "Maryland") +
  geom_text(data = new_data, aes(x=drain_area_va, y=discharge_perDA,label=site_no), 
                  size=3, hjust=c(1,1,0), vjust=-0.5)

# ggsave(graph_1, filename = "All_peaks_shaded.png", width = 4.5, height = 4.5)

library(readxl)
gagesII <- read_excel("flood_freq/gagesII_sept30_2011_conterm.xlsx", sheet = "LC06_Basin")

graph_data <- graph_data %>%
  left_join(select(gagesII, site_no=STAID, develop=DEVOPENNLCD06), by="site_no")

graph_data$develop[is.na(graph_data$develop)] <- mean(graph_data$develop, na.rm = TRUE)

graph_2 <- ggplot(data = graph_data) +
  geom_point( aes(x=drain_area_va, y=discharge_perDA, 
                  color = fromServices, alpha = peak_dt, size = develop)) +
  scale_y_log10(limits = c(1,10000)) +
  scale_x_log10(limits = c(.1,100000)) +
  scale_color_manual(values = c("red","black")) +
  theme(legend.position = "none") +
  xlab("Drainage area in square miles") +
  ylab("Discharge in cubic feet per second per square mile") +
  ggtitle("Full period of record", subtitle = "Maryland") +
  geom_text(data = new_data, aes(x=drain_area_va, y=discharge_perDA,label=site_no), 
            size=3, hjust=c(1,1,0), vjust=-0.5)

# ggsave(graph_2, filename = "All_peaks_shaded_sized.png", width = 4.5, height = 4.5)

graph_3 <- ggplot(data = graph_data) +
  geom_point( aes(x=drain_area_va, y=discharge_perDA, 
                  color = fromServices, alpha = peak_dt, size = develop)) +
  scale_y_log10(limits = c(1,10000)) +
  scale_x_log10(limits = c(.1,100000)) +
  scale_color_manual(values = c("red","black")) +
  theme(legend.position = "none") +
  xlab("Drainage area in square miles") +
  ylab("Discharge in cubic feet per second per square mile") +
  ggtitle("Full period of record", subtitle = "Maryland") +
  geom_text(data = new_data, aes(x=drain_area_va, y=discharge_perDA,label=site_no), 
            size=3, hjust=c(1,1,0), vjust=-0.5)

# ggsave(graph_3, filename = "All_peaks_shaded_sized.png", width = 4.5, height = 4.5)


library(ggmap)
library(maps)
library(mapdata)

states <- map_data("state")
md_line <- states[states$region %in% "maryland",]

graph_data <- graph_data %>%
  left_join(select(md_sites, dec_long_va, dec_lat_va, site_no), by="site_no")

graph_data$dec_long_va[graph_data$site_no == "Tiber"] <- -(76 + 48/60 + 10.4/3600)
graph_data$dec_lat_va[graph_data$site_no == "Tiber"] <- 39 + 16/60 + 0.8/3600
graph_data$dec_long_va[graph_data$site_no == "Hudson"] <- -(76 + 48/60 + 35.1/3600)
graph_data$dec_lat_va[graph_data$site_no == "Hudson"] <- 39 + 16/60 + 7.6/3600
graph_data$dec_long_va[graph_data$site_no == "New Cut"] <- -(76 + 47/60 + 50.5/3600)
graph_data$dec_lat_va[graph_data$site_no == "New Cut"] <- 39 + 15/60 + 57.8/3600

map_md <- ggplot() + 
  geom_polygon(data = md_line, aes(x = long, y = lat, group = group), 
               fill = "grey", color = "black") + 
  coord_fixed(1.3) +
  theme_nothing() +
  scale_color_manual(values = c("red","black")) +
  geom_point(data = graph_data, 
             aes(x=dec_long_va, y=dec_lat_va, 
                 color = fromServices, alpha = peak_dt, size = develop))

# ggsave(map_md, filename = "md_map.png", width = 4.5, height = 4.5)


library(grid)
vp <- viewport(width = 0.45, height = 0.3, 
               x = 0.76, y = 0.8)

# png("combo.png",width = 8, height = 8, units = "in", res = 200)
# print(graph_3)
# print(map_md, vp = vp)
# dev.off()


graph4 <- ggplot(data = graph_data) +
  geom_point( aes(x=drain_area_va, y=discharge_perDA, 
                  color = fromServices, alpha = peak_dt, size = develop)) +
  scale_y_log10(limits = c(1,10000)) +
  scale_x_log10(limits = c(.1,100000)) +
  scale_color_manual(values = c("red","black")) +
  theme(legend.position = "none") +
  xlab("Drainage area in square miles") +
  ylab("Discharge in cubic feet per second per square mile") +
  ggtitle("Full period of record", subtitle = "Maryland") +
  geom_text(data = new_data, aes(x=drain_area_va, y=discharge_perDA,label=site_no), 
            size=3, hjust=c(1,1,0), vjust=-0.5) +
  geom_point(aes(x=drain_area_va, y=yr100), color = "blue") +
  geom_line(data = filter(graph_data, !is.na(yr100)), aes(x=drain_area_va, y=yr100), color = "blue", size = 1) +
  geom_point(aes(x=drain_area_va, y=yr200), color = "blue") +
  geom_line(data = filter(graph_data, !is.na(yr200)), aes(x=drain_area_va, y=yr200), color = "blue", size = 1) +
  geom_point(aes(x=drain_area_va, y=yr500), color = "blue") +
  geom_line(data = filter(graph_data, !is.na(yr500)), aes(x=drain_area_va, y=yr500), color = "blue", size = 1)

library(grid)
vp <- viewport(width = 0.45, height = 0.3, 
               x = 0.76, y = 0.8)

# png("combo2.png",width = 8, height = 8, units = "in", res = 200)
# print(graph4)
# print(map_md, vp = vp)
# dev.off()


as.Date_origin <- function(x){
  as.Date(x, origin = '1970-01-01')
}

flood_text <- data.frame(drain_area_va = .1,
                         discharge_perDA = c(yr100[1], yr200[1],yr500[1]),
                         text_gr = c("100","200","500"),
                         stringsAsFactors = FALSE)

seg_data <- data.frame(drain_area_va = rep(c(.1, drain_area_va[1]),3),
                       grouping = c("a","a","b","b","c","c"),
                       seg_y = c(yr100[1],yr100[1], yr200[1],yr200[1], yr500[1], yr500[1]),
                       stringsAsFactors = FALSE)
                       

graph5 <- ggplot(data = graph_data) +
  geom_point( aes(x=drain_area_va, y=discharge_perDA, 
                  color = fromServices, alpha = peak_dt, size = develop)) +
  scale_y_log10(limits = c(10,10000)) +
  scale_x_log10(limits = c(.1,100000)) +
  scale_alpha_continuous(breaks = c(-20000, -10000, 0, 10000),
                         labels = c("Before 1915", "1942", "1970","After 1997")) +#) +#labels=as.Date_origin, ) +
  guides(color = FALSE,
         alpha = guide_legend(title = "Peak Date"),
         size = guide_legend(title = "Developed\nLand Use %")) +
  scale_color_manual(values = c("red","black")) +
  xlab("Drainage area in square miles") +
  ylab("Discharge in cubic feet per second per square mile") +
  ggtitle("Full period of record", subtitle = "Maryland") +
  geom_text(data = new_data, aes(x=drain_area_va, y=discharge_perDA,label=site_no), 
            size=3, hjust=c(1,1,0), vjust=-0.5) +
  geom_point(aes(x=drain_area_va, y=yr100), color = "blue") +
  geom_line(data = filter(graph_data, !is.na(yr100)), aes(x=drain_area_va, y=yr100), color = "blue", size = 1) +
  geom_point(aes(x=drain_area_va, y=yr200), color = "blue") +
  geom_line(data = filter(graph_data, !is.na(yr200)), aes(x=drain_area_va, y=yr200), color = "blue", size = 1) +
  geom_point(aes(x=drain_area_va, y=yr500), color = "blue") +
  geom_line(data = filter(graph_data, !is.na(yr500)), aes(x=drain_area_va, y=yr500), color = "blue", size = 1) +
  geom_text(data = flood_text, aes(x=drain_area_va, y=discharge_perDA, label = text_gr), color="blue", hjust=1) +
  geom_line(data = seg_data, aes(x=drain_area_va, y=seg_y, group = grouping), color = "blue", linetype = 2)

library(grid)
vp <- viewport(width = 0.45, height = 0.3, 
               x = 0.6, y = 0.8)

png("combo3.png",width = 8, height = 8, units = "in", res = 200)
print(graph5)
print(map_md, vp = vp)
dev.off()

