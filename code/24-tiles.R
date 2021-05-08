# 30DayChartChallenge
# Day 24: time-series: tiles
# Kaizad F. Patel
# 2021-05-02

# Use BBWM soil temperature data

##
library(tidyverse)
library(RColorBrewer)

sysfonts::font_add_google("Patua One", "Patua")
sysfonts::font_add_google("Lato", "Lato")
sysfonts::font_add_google("Barlow", "Barlow")
sysfonts::font_add_google("Merriweather", "Merriweather")
showtext::showtext_auto()


## load and clean data
bbwm_data = read.delim("data/24-PANGAEA.885853.txt")

bbwm_data_cleaned = 
  bbwm_data %>% 
  rename(air = `TTT.monthly.m...C.`,
         organic = `T.soil...C...organic.soil.`,
         mineral = `T.soil...C...mineral.soil.at.10.cm.depth.`,
         mineral25 = `T.soil...C...mineral.soil.at.25.cm.depth.`) %>% 
  separate(`Date.Time`, sep = "-", into = c("Year", "Month"))


## plot
bbwm_data_cleaned %>% 
  filter(!is.na(organic)) %>% 
  ggplot(aes(x = Year, y = reorder(Month, desc(Month)), fill = organic))+
  geom_tile(alpha = 0.9, color = "white", size = 1)+
  scale_fill_gradientn(colors = soilpalettes::soil_palette("redox2",5))+
#  scale_fill_gradientn(colors = rev(brewer.pal(5, "RdBu")))+
#  scale_fill_gradientn(colors = (brewer.pal(5, "Blues")))+
#  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0)+
#  scale_y_reverse()+
  labs(x = "", y = "Month", fill = "°C",
       title = "Soil Temperature",
       subtitle = "Bear Brook Watershed in Maine, organic soil",
       caption = "github.com/kaizadp
           data: Bear Brook Watershed in Maine
           #30DayChartChallenge 2021: 24-Tiles")+
  scale_x_discrete(breaks = seq(2001, 2016, 5))+
  coord_equal()+
  theme_classic()+
  theme(axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_text(family = "Barlow"),
        axis.title = element_text(family = "Barlow"),
        plot.title = element_text(hjust = 0.5, family = "Patua", size = 15),
        plot.subtitle = element_text(hjust = 0.5, family = "Lato", size = 10),
        plot.caption = element_text(hjust = 0.5, family = "Lato", size = 7),
        
        
        legend.position = "right"
        )

ggsave("images/24-tiles.png", height = 4, width = 5)
