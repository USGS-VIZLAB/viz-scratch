
# plotting dataRetrieval downloads ----------------------------------------

library(tidyverse)
library(ctv)
library(packageRank)

## hydrology task view packages
hyd_packages <- ctv:::.get_pkgs_from_ctv_or_repos(views = "Hydrology") %>% 
  unlist(use.names = FALSE)

# get timeseries of downloads for all hydrology packages
dl_list <- lapply(hyd_packages, function(x)cranDownloads(packages = x, from = "2012-10-01", to = Sys.Date()))

list_out<-NULL
for (i in 1:length(hyd_packages)){
  list_out <- rbind(list_out, dl_list[[i]]$cranlogs.data)
}
list_max <- list_out%>%group_by(package)%>%summarize(end = max(cumulative))

## plot timeseries of all packages with dataRetrieval highlighted
list_out %>%
  ggplot(aes(date, cumulative, group=package))+
  geom_line(alpha=.5, 
            color="grey", 
            size=.7)+
  theme_classic(base_size=46)+
  theme(axis.line = element_line(color="darkgrey", size=2),
        axis.text.y.right = element_text(
          color=c("grey","grey","dodgerblue", "grey", "grey",  "grey"),
          hjust=1),
        axis.text.x = element_text(color="grey"),
        axis.ticks=element_blank())+
  labs(x='', y='')+
  geom_line(data=.%>% filter(package=='dataRetrieval'), 
            alpha=1, 
            color="dodgerblue", 
            size=1)+
  geom_text(data = list_out %>% 
              group_by(package) %>% 
              summarize(end = max(cumulative)) %>% 
              filter(end > 99000) %>% 
              arrange(desc(end)),
            aes(label = package, x=Sys.Date(), y = end), 
            color=c( "grey", "grey", "grey","grey","dodgerblue"),
            hjust=1.25,vjust = -0.5,
            size=10,
            angle = c(45, 30, 30, 30, 20))+
  scale_y_continuous(breaks=scales::breaks_pretty(n=6), 
                     labels=scales::label_number(scale=1/1000, suffix = "k"),
                     position="right", expand = c(0, 0))+
  scale_x_date(breaks=scales::breaks_width("2 year"),
               labels=scales::label_date_short(), 
               expand=c(0,0))
ggsave("dataRetrieval_100k.png", width = 12, height = 6.75)
