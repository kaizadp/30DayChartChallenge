# 30DayChartChallenge
# Day 1: part-to-whole
# Kaizad F. Patel
# 2021-04-01

##

library(tidyverse)
library(ggrepel)

sysfonts::font_add_google("Patua One", "Patua")
sysfonts::font_add_google("Lato", "Lato")
showtext::showtext_auto()

##

#raininspain = 
tribble(
  ~where, ~perc, ~y, 
  "in the plain", 86, 5,
  "not in the plain", 10, 5
) %>% 
  ggplot(aes(y = perc, x = y, fill = where))+
  geom_bar( stat = "identity")+
  
  geom_text_repel(data = . %>% filter(where == "in the plain"),
                  aes(label = where, x = 5, y = 90),
                  nudge_x = 1.5, 
                  nudge_y = -5,
                  box.padding = 1,
                  segment.curvature = -0.1,
                  hjust = 1,
                  family = "Lato")+
  
  geom_text_repel(data = . %>% filter(where == "not in the plain"),
                  aes(label = where, x = 5, y = 5),
                  nudge_x = -1.5, 
                  nudge_y = 0,
                  box.padding = 1,
                  segment.curvature = -0.1,
                  hjust = 1,
                  family = "Lato")+
  
  coord_polar(theta = "y")+
  scale_fill_manual(values = PNWColors::pnw_palette("Bay", 3))+
  xlim(0,6)+
  labs(title = "Where is the rain in Spain?",
       subtitle = "mainly in the plain",
       caption = "github.com/kaizadp | source: My Fair Lady \n 
       #30DayChartChallenge | 1: part-to-whole \n")+
  theme_void()+
  theme(legend.position = "none",
        panel.background = element_rect(fill = "#F9F8F1", color = NA),
        plot.background = element_rect(fill = "#F9F8F1", color = NA),
        plot.title = element_text(hjust = 0.5, family = "Patua", face = "bold", size = 25),
        plot.subtitle = element_text(hjust = 0.5, family = "Lato", face = "italic", size = 22),
        plot.caption = element_text(hjust = 0.5, family = "Lato", color = "grey50"))+
  NULL

ggsave("images/1-part_to_whole.png")

