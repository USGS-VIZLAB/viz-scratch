
# scrape NIFC fire statistics  --------------------------------------------

library(tidyverse);library(rvest);library(stringr)

## scrape web data to table
fire_cost <- read_html('https://www.nifc.gov/fire-information/statistics/suppression-costs') %>%
  html_table(header = TRUE) %>%
  as.data.frame() %>%
  mutate(across(everything(), ~as.numeric(gsub("\\$", "", str_replace_all(.x, "[[:punct:]]", ""))))) %>% # convert to numeric
  filter(!is.na(Year))  # remove total rows
fire_cost

# plot wildfire area by cost ----------------------------------------------

library(scico);library(scales)

# custom dark theme
theme_fire_dark <- function(){
  theme_classic(base_size=16)+
    theme(plot.background = element_rect(fill="grey20"),
          panel.background = element_rect(fill="grey20", color=NA),
          axis.line=element_line(color="white", size =2, lineend = "round"),
          axis.text=element_text(color="grey90", face="italic"),
          axis.text.y=element_blank(),
          axis.ticks=element_blank(),
          axis.title = element_text(color="white"),
          panel.grid.major = element_line(color="white", linetype = "dotted", size=.5),
          plot.title = element_text(color="white", , face="bold"),
          plot.subtitle = element_text(color="white", face="italic"),
          legend.background = element_blank(),
          legend.text = element_text(color="white"),
          legend.title = element_text(color="White"),
          plot.margin = margin(1, 1, 1, 1, "cm"))
}

## fake data to use for axis.text placement
labs <- data.frame(Acres = c(1000000, 1000000, 1000000),
                   Total = c(1100000000, 2100000000, 3100000000),
                   lab = c("$1,000M", "$2,000M", "$3,000M"))

fire_cost %>%
  ggplot(aes(Acres, Total, color = Acres))+
  geom_point(aes(size=Fires), shape=16)+
  geom_text(data=labs, aes(label=lab), color="grey90", fontface="italic", size=4.5)+
  theme_fire_dark()+
  scale_size(range=c(2,8), 
             labels = scales::label_comma(), 
             breaks=breaks_pretty(n=6))+
  scale_color_scico(palette="lajolla", end=0.7)+
  labs(x="", y="")+
  scale_y_continuous(labels=scales::dollar_format(scale=1/1000000, suffix = "M"))+ # in millions of dollars
  scale_x_continuous(labels=scales::label_number(scale=1/1000000, suffix = "M"))+
  ggtitle("Millions of acres burned &\nmillions of dollars spent suppressing wildfires",
          subtitle = "1985 - 2020")+
  guides(size=FALSE,color=FALSE)

ggsave("wilfire_acres_by_dollars.png", width = 16, height = 9)

# animate through time ----------------------------------------------------

library(gganimate)

fire_ani <- fire_cost %>% 
  ggplot(aes(Acres, Total, color = Acres))+
  geom_point(aes(size=Fires), shape=16)+
  geom_text(aes(label="{Year}", y=3100000000, x=10000000), size=8, color="white")+
  geom_text(data=labs, aes(label=lab), color="grey90", fontface="italic", size=4.5)+
  theme_fire_dark()+
  scale_size(range=c(2,8), 
             labels = scales::label_comma(), 
             breaks=breaks_pretty(n=6))+
  scale_color_scico(palette="lajolla", end=0.7)+
  labs(x="", y="")+
  scale_y_continuous(labels=scales::dollar_format(scale=1/1000000, suffix = "M"))+ # in millions of dollars
  scale_x_continuous(labels=scales::label_number(scale=1/1000000, suffix = "M"))+
  ggtitle("Millions of acres burned &\nmillions of dollars spent suppressing wildfires ")+
  guides(size=FALSE,color=FALSE)+
  transition_states(
    Year,
    transition_length = 2,
    state_length = 2
  ) +
  enter_fade() +
  shadow_mark()

animate(fire_ani, width=800, height = 450)
anim_save("fire_animated_time.gif")
