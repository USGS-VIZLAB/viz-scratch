# Load Packages
library(tidyverse)
library(ggplot2)
library(grid)
library(gridExtra)
library(ggpubr)

theme_set(theme_classic(base_size = 14))

# Read Data
dir <- "/Users/ebechtel/git/viz-scratch/gage_streamorder"
output_dir <- file.path(dir, "3_visualize/out")
setwd(dir)
gage_orig <- read_csv("/data/CASC_nhd.csv") # gage_df provided by Colleen
gage_df <- read_csv('/data/CASC_nhd_all.csv')# gage_df provided by Colleen


# Plot Data

## general styles
headwaters <- c(
  "1" = "coral",
  "2" = "coral",
  "3" = "coral",
  "4" = "grey",
  "5" = "grey",
  "6" = "grey",
  "7" = "grey",
  "8" = "grey",
  "9" = "grey",
  "10" = "grey"
)

## aggregated
plot_1 <- function(file_in) {
  file_in %>%
    group_by(streamorde) %>%
    summarize(no_casc = sum(no), yes_casc = sum(yes)) %>% 
    ggplot() +
    geom_bar(stat="identity", aes(x = streamorde, y = yes_casc), fill = "grey") +
    scale_x_continuous(breaks = 1:9)+
    theme_classic(base_size = 11) +
    labs(x = "Stream order", 
         y= "Gaged reaches")+
    theme(legend.position = "none") 
}
plot_1(gage_df)

## facets of gaged comids
plot_facet_gaged <- function(file_in){
  file_in %>%
    ggplot() +
    geom_bar(stat="identity", 
             aes(x = streamorde, y = yes, fill = factor(streamorde))) +
    scale_fill_manual(values = headwaters)+
    scale_x_continuous(breaks = 1:9)+
    scale_y_continuous(labels = scales::label_number(big.mark =','), breaks = c(0,250,500), limits = c(0,500))+
    theme_classic(base_size = 11) +
    labs(x = "Stream order", 
         y= "Gaged reaches")+
    theme(legend.position = "none", 
          strip.background = element_blank())+
    facet_wrap(~CASC, scales="free", strip.position = "bottom")
}
plot_facet_gaged(gage_df)


## facets of hist of total comids
plot_facet_totals <- function(file_in){
  file_in %>%
    ggplot() +
    geom_bar(stat="identity",
             aes(x = streamorde, y = total_comid, fill = factor(streamorde))) +
    scale_fill_manual(values = headwaters)+
    scale_x_continuous(breaks = 1:9)+
    scale_y_continuous(labels = scales::label_number(big.mark =','), breaks = c(0, 100000, 200000), limits = c(0, 250000))+
    theme_classic(base_size = 11) +
    labs(x = "Stream order", 
         y= "Gaged reaches")+
    theme(legend.position = "none", 
          strip.background = element_blank())+
    facet_wrap(~CASC,
               scales="free", 
               strip.position = "bottom"
               )
}
plot_facet_totals(gage_df)


## Double-axis bar charts
plot_casc_compare <- function(file_in, output_dir){
  
  title <- file_in$CASC[1]
  
  g.mid <- file_in %>%
    ggplot(aes(x=1, y=streamorde)) +
    geom_text(aes(label=streamorde))+
    ggtitle("Streamorder")+
    ylab(NULL)+
    scale_x_continuous()+
    scale_y_reverse()+
    theme(axis.title=element_blank(),
          panel.grid=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          panel.background=element_blank(),
          axis.text.x=element_text(color=NA),
          axis.ticks.x=element_line(color=NA),
          axis.line.y = element_blank(),
          plot.title = element_text(hjust = 0.5, size = 10),
          plot.margin = unit(c(1,-1,1,-1), "mm"))
    
    
  g1 <- file_in %>%
    ggplot()+
    geom_bar(stat="identity",
             aes(x = streamorde, y = total_comid, fill = factor(streamorde)))+
    scale_fill_manual(values = headwaters)+
    xlab("Count")+
    ggtitle("Total Comids")+
    theme(axis.title.x = element_blank(), 
          axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.line.y = element_blank(),
          legend.position = "none",
          plot.title = element_text(hjust = 0.5, face = "bold"),
          plot.margin = unit(c(1,-1,1,0),"mm")) +
    scale_y_reverse() +
    scale_x_reverse() +
    coord_flip()
  g1
  
  g2 <- file_in %>%
    ggplot() +
    geom_bar(stat="identity",
             aes(x = streamorde, y = yes, fill = factor(streamorde)))+
    scale_fill_manual(values = headwaters)+
    ggtitle("Gaged Comids")+
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.line.y = element_blank(),
          legend.position = "none",
          plot.title = element_text(hjust = 0.5, face = "bold"),
          plot.margin = unit(c(1,0,1,-1), "mm")) +
    scale_x_reverse()+
    coord_flip()
  g2
  
  ### arrange
  grid.arrange(g1, g.mid, g2, ncol=3, widths=c(4/9,1/9,4/9))

  
  
  ### save image of plot - this is not working
  png(file = file.path(output_dir, "casc.png"), width=11.5, height=8.5, res=150, units ="in")
}

# Manually filter for each CASC and save manually, since the automatic save above is not working
this_casc <- gage_df %>%
  filter(CASC == "South Central") %>%
  arrange(streamorde)

plot_casc_compare(this_casc, output_dir)
