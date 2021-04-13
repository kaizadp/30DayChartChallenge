# 30DayChartChallenge
# Day 13: correlations
# Kaizad F. Patel
# 2021-04-13

##
# Spurious Correlations by Tyler Vigen https://www.tylervigen.com/spurious-correlations

library(tidyverse)
theme_set(theme_void())

sysfonts::font_add_google("Patua One", "Patua")
sysfonts::font_add_google("Barlow", "Barlow")
showtext::showtext_auto()

maine_correl = 
  tribble(
    ~margarine, ~divorce, ~year1,
    8.2, 5, 2000,
    7, 4.7, 2001,
    6.5, 4.6, 2002,
    5.3, 4.4, 2003,
    5.2, 4.3, 2004, 
    4, 4.1, 2005,
    4.6, 4.2, 2006, 
    4.5, 4.2, 2007, 
    4.2, 4.2, 2008, 
    3.7, 4.1, 2009
  )

maine_correl %>% 
  mutate(year = as.integer(year1)) %>% 
  ggplot()+
  geom_path(aes(x = year, y = divorce), color = "#0077b6", size = 1)+ #blue
  geom_path(aes(x = year, y = (margarine+13)/4), color = "#ef476f", size = 1)+
  geom_point(aes(x = year, y = divorce), color = "#0077b6", shape = 21, stroke = 1, size = 2.5, fill = "white")+
  geom_point(aes(x = year, y = (margarine+13)/4), color = "#ef476f", shape = 21, stroke = 1, size = 2.5, fill = "white")+
  
  annotate("text", label = "divorces in Maine, \nper 1000 people", x = 2002, y = 4.2, size = 4, color = "#0077b6", family = "Barlow")+
  annotate("text", label = "lbs of margarine consumed", x = 2003.2, y = 5.2, size = 4, color = "#ef476f", hjust = 0, family = "Barlow")+
  
  annotate("curve", x = 2001, y = 4.3, xend = 2002, yend = 4.5, curvature = -0.3, color = "#0077b6", arrow = arrow(length = unit(0.3, "cm")))+
  annotate("curve", x = 2003, y = 5.2, xend = 2002, yend = 5, curvature = 0.3, color = "#ef476f", arrow = arrow(length = unit(0.3, "cm")))+
  
  
  scale_y_continuous(limits = c(4,5.5), breaks = seq(2,6, by = 0.5), name = "margarine consumed, lbs", 
                     sec.axis = sec_axis(~.*4 -13, name = "divorces per 1000 people"))+
  scale_x_continuous(labels = scales::number_format(accuracy = 1, big.mark = ""))+
  
  labs(x = "",
       title = "Less margarine, fewer divorces in Maine",
       subtitle = "Spurious Correlations",
       caption = "github.com/kaizadp | source: Spurious Correlations by @TylerVigen \n#30DayChartChallenge 2021 | 13: correlation")+
  
  theme_classic()+
  theme(axis.title.y = element_text(hjust = 0, margin = margin(0, 10, 0, 0), family = "Barlow"),
        axis.title.y.right = element_text(hjust = 0, margin = margin(0, 0, 0, 10), family = "Barlow"),
        axis.text = element_text(family = "Barlow"),
        plot.title = element_text(hjust = 0.5, family = "Patua", size = 16),
        plot.subtitle = element_text(hjust = 0.5, family = "Barlow", size = 16),
        plot.caption = element_text(hjust = 0.5, color = "grey30", family = "Barlow", size = 8),
        plot.background = element_rect(fill = "#FFF9EB"),
        panel.background = element_rect(fill = "#FFF9EB")
        )+
  NULL


ggsave("images/13-correlation.png", height = 6, width = 6.3)
