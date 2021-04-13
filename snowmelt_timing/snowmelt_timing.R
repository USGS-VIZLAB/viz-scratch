library(RNRCS)
library(tidyverse);library(reshape2);library(lubridate)


# fetch SNOTEL site data -------------------------------------------------------

# metadata for all SNOTEL sites
meta <- grabNRCS.meta('SNTL')

## filter to currently active sites
sntl_meta <- meta$SNTL %>% 
  mutate(sntl_id = site_id, site_id = gsub("SNTL:", "", site_id)) %>%
  distinct(sntl_id, site_id, wyear, start, enddate) %>% 
  filter(enddate == '2100-January')

# fetch SNOTEL SWE data ---------------------------------------------------

# pull SWE up to current date
get_SWE_2021 <- function(site, date_today) {
  
  date <- as.Date(date_today)
  site_meta <- sntl_meta %>% filter(site_id == site)
  year <- word(site_meta$start, 1,1, sep="-")
  sitey <- gsub('SNTL:', '', site_meta$site_id)
  
  ## pull data for nrcs historical range
  sntl <- grabNRCS.data(network = 'SNTL', 
                        site_id = sitey, 
                        timescale = 'daily', 
                        DayBgn = sprintf("%s-10-01", year),
                        DayEnd = date)
  print(i)
  
  if("Snow.Water.Equivalent..in..Start.of.Day.Values" %in% colnames(sntl)){
    
    sntl_clean <- clean_SWE(today = sntl) %>% mutate(site_id = sitey)
    write.csv(sntl_clean, sprintf('swe_%s.csv', sitey),row.names=FALSE)
    return(sntl_clean)
    
  }
}

clean_SWE <- function(today){
  
  # organize data
  site_curve <- today %>%
    mutate(date = as.Date(Date), 
           year = year(date), month = month(date), 
           water_year = year(date) + ifelse(month(date) >= 10, 1, 0), 
           water_day = (date - as.Date(sprintf('%s-10-01', water_year)))) %>%
    group_by(water_year) %>%
    mutate(water_day_max = min(water_day)) %>% ungroup() %>%
    mutate(water_day = as.numeric(water_day - water_day_max + 1)) %>%
    ungroup() %>%
    mutate(swe = Snow.Water.Equivalent..in..Start.of.Day.Values) %>%
    dplyr::select(date, water_year, water_day, swe, year) 
  
  return(site_curve)
}

## pull SNOTEL data for all currently active sites from inception to today
site_list <- unique(sntl_meta$site_id)

## this takes A WHILE!
for (i in 1:length(site_list)) {
  swe_out <- tryCatch(get_SWE_2021(site=site_list[i], date_today = Sys.Date()),
                      error = function(e) return())
}


# calculate snowmelt timing -----------------------------------------------

# read in data
swe_files <- list.files(pattern="swe", full.names=TRUE)

swe_data <- lapply(swe_files, read_csv) %>% 
  bind_rows() %>%
  filter(water_year >= 1981 & water_year <= 2020)

# find 50% of peak swe for each year and site
peaks <- swe_data %>%
  group_by(site_id, water_year)%>%
  summarize(peak_swe = max(swe, na.rm=TRUE)) %>%
  mutate(sm50_swe = peak_swe/2)

# find date of peak swe each year
# same as SNOTEL https://www.wcc.nrcs.usda.gov/webmap/help/Definitions.pdf
peak_dates <- swe_data %>%
  left_join(peaks) %>%
  group_by(site_id, water_year)%>%
  filter(swe == peak_swe)%>%
  filter(water_day == max(water_day, na.rm=TRUE))%>% # take latest date peak occurs
  mutate(peak_day = water_day)%>%
  dplyr::select(-date, -water_day, -swe, -year) %>%
  ungroup()

# find date of sm50_swe for each year
sm50_dates <- swe_data %>%
  left_join(peak_dates) %>%
  group_by(site_id, water_year)%>%
  filter(water_day >= peak_day) %>%
  filter(swe <= sm50_swe) %>%
  filter(water_day == min(water_day, na.rm=TRUE)) %>% # earliest water day at or below peak/2
  mutate(sm50_day = water_day) %>%
  dplyr::select(-date, -water_day, -swe)

## difference from median date in days
melt_days <- sm50_dates %>%
  left_join(sm50_dates %>% group_by(site_id) %>%
              summarize(median_date  =  median(sm50_day),
              yrs = length(unique(water_year)))) %>%
  mutate(sm50_diff = sm50_day - median_date)


# make chart --------------------------------------------------------------

library(scico)

melt_days %>%
  filter(sm50_diff > -100 & sm50_diff < 100 & yrs > 40) %>%
  ggplot(aes(water_year, sm50_diff, group=site_id))+
  geom_point(aes(color=sm50_diff), alpha=.7, position=position_jitter(0.3), shape=21, size=1)+
  theme_classic()+
  coord_flip()+
  labs(y="<< early melt                later melt >>", x="")+
  ggtitle("Snowmelt timing (1981 - 2021)", subtitle="compared to the median snowmelt date at NRCS snow telemetry (SNOTEL) site")+
  scale_color_scico(palette="brocO")+
  geom_hline(yintercept=0, color="black")+
  theme(axis.line= element_blank(),
        axis.text  = element_text(color="grey", size=16),
        axis.title  = element_text(color="grey", size=16),
        legend.position="none",
        panel.grid.major = element_line(linetype="dotted", color="grey"),
        axis.ticks = element_blank())+
  scale_y_continuous(position = "left",
                     breaks=c(-100, -50, 0, 50, 100),
                     labels=c("-100 days", "-50 days", "0", "+50 days", "+100 days"))+
  scale_x_continuous(position = "top", 
                     trans = "reverse")

ggsave("snowmelt_timing.png", width=10, height=7)
