# 30DayChartChallenge
# Day 19: time-series: global change
# Kaizad F. Patel
# 2021-05-02

## Plotting the Keeling Curve
## Data: https://keelingcurve.ucsd.edu
##

library(tidyverse)
library(ggforce)
theme_set(theme_void())

sysfonts::font_add_google("Patua One", "Patua")
sysfonts::font_add_google("Lato", "Lato")
sysfonts::font_add_google("Barlow", "Barlow")
sysfonts::font_add_google("Merriweather", "Merriweather")
showtext::showtext_auto()

## load data
keeling_data = read.csv("data/monthly_in_situ_CO2_mlo.csv")

## process data
keeling_processed = 
  keeling_data %>% 
  # remove all 0 and -ve values
  filter(CO2_ppm > 0) %>% 
  # create date
  mutate(Date = paste0(Yr, "-", Mn, "-01"),
         Date = lubridate::ymd(Date))

## labels for historical events
keeling_dat = 
  tribble(
    ~Date, ~y, ~event,
    "1977-05-25", 336, "Star Wars Ep. 4",
    "1989-11-09", 351, "Berlin Wall falls",
    "2007-06-29", 386, "first iPhone",
    "2020-12-01", 414, "today"
  ) %>% 
  mutate(Date = lubridate::ymd(Date))


## plot
keeling_processed %>% 
  ggplot(aes(x = Date, y = CO2_ppm))+
  geom_path()+
  geom_point(size = 0.5)+
  
  geom_mark_circle(data = keeling_dat,
                   aes(y = y, group = event, label = event), 
                   expand = unit(3, "mm"), color = "darkred", fill = "red", 
                   label.fontsize = 8, label.hjust = 0,
                   label.fill = NA, label.colour = "darkred", label.family = "Lato", 
                   con.size = 0.5, con.border = "one", con.colour = "darkred",
                   con.cap = 0, con.type = "elbow")+
  
  annotate("text", label = "The Keeling Curve", 
           x = as.Date("1958-05-05"), y = 410, hjust = 0, family = "Patua", size = 8)+
  annotate("text", label = "Global atmospheric carbon dioxide concentrations", 
           x = as.Date("1959-05-05"), y = 403, hjust = 0, family = "Barlow", size = 3)+
  
  labs(x = "",
       y = expression(bold("CO"[2] * ", ppm")),
       caption = "github.com/kaizadp
       data: Scripps Institute of Oceanography
       #30DayChartChallenge 2021: 19-Global Change")+
  theme_classic()+
  theme(plot.background = element_rect(fill = "#FFF9D6"),
        panel.background = element_rect(fill = "#FFF9D6"),
        axis.text = element_text(color = "black"),
        plot.caption = element_text(margin = margin(-60, 0, 30, 0),
                                    size = 8, family = "Barlow", color = "grey50"))+
  NULL


ggsave("images/19-global_change.png", height = 5, width = 5)
