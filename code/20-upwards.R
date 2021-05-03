# 30DayChartChallenge
# Day 20: time-series: upwards
# Kaizad F. Patel
# 2021-05-02

##

library(tidyverse)
library(ggforce)
library(RColorBrewer)
theme_set(theme_void())

sysfonts::font_add_google("Patua One", "Patua")
sysfonts::font_add_google("Lato", "Lato")
sysfonts::font_add_google("Barlow", "Barlow")
sysfonts::font_add_google("Merriweather", "Merriweather")
showtext::showtext_auto()

usa_ne_anomaly_data = read.table("data/20-usa_ne_anomaly.txt", sep = ",", header = TRUE)
usa_ne_anomaly_processed = 
  usa_ne_anomaly_data %>% 
  rename(CalYr = `Calendar.Year..Jan.Dec.`)

usa_ne_anomaly_processed %>% 
  filter(!is.na(CalYr)) %>% 
  ggplot(aes(xend = Year, x = Year, y = 1, yend = 2, fill = CalYr, color = CalYr))+
  geom_segment(size = 5)+
  scale_color_gradientn(colors = rev(brewer.pal(11, "RdBu")))+
  theme_void()+
  theme(legend.position = "none")


usa_ne_anomaly_processed %>% 
  filter(!is.na(CalYr)) %>% 
  ggplot(aes(x = Year, y = 1, fill = CalYr))+
  geom_tile()+
  scale_fill_gradientn(colors = rev(brewer.pal(11, "RdBu")))+
 
  annotate("rect", xmin = 1894.5, xmax = 2021, ymin = 0.62, ymax = 0.9, fill = "white", alpha = 0.8)+
  annotate("text", label = "THE WARMING STRIPES", x = 1895, y = 0.8, hjust = 0,
           size = 6, family = "Patua")+
  annotate("text", label = "Northeastern USA", x = 1895, y = 0.7, hjust = 0,
           size = 4, family = "Barlow", color = "grey10")+
  
  annotate("text", label = "github.com/kaizadp
           data: UMaine Climate Reanalyzer
           #30DayChartChallenge 2021: 20-Upwards", 
           x = 2019, y = 0.76, hjust = 1, vjust = 0.5,
           size = 2, family = "Barlow", color = "grey15")+
  expand_limits(x = 1896)+
  theme_void()+
  theme(legend.position = "none")

ggsave("images/20-upwards.png", width = 7.81, height = 2.69)
