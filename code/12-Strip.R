# 30DayChartChallenge
# Day 12: strip
# Kaizad F. Patel
# 2021-04-11

##
# https://www.kaggle.com/deepu1109/star-dataset

library(tidyverse)
library(ggforce)
theme_set(theme_void())

sysfonts::font_add_google("Patua One", "Patua")
sysfonts::font_add_google("Lato", "Lato")
sysfonts::font_add_google("Barlow", "Barlow")
sysfonts::font_add_google("Merriweather", "Merriweather")
showtext::showtext_auto()


star_data = read.csv("data/12-strip_starclass.csv")
star_data2 = 
  star_data %>% 
  mutate(color = recode(`Spectral.Class`, 
                        "O" = "#045a8d", "B" = "#74a9cf", 
                        "A" = "#bdc9e1", "F" = "#f1eef6", 
                        "G" = "#ffffb2", "K" = "#fd8d3c", 
                        "M" = "#b30000"))


star_label = 
  star_data2 %>% 
  dplyr::select(color, `Spectral.Class`) %>% 
  distinct() %>% 
  rename(SpectralClass = `Spectral.Class`) %>% 
  mutate(SpectralClass = factor(SpectralClass, levels = c("O", "B", "A", "F", "G", "K", "M"))) %>% 
  arrange(SpectralClass) %>% 
  mutate(x = c(20000, 22000, 24000, 26000, 28000, 30000, 32000))



star_data2 %>% 
  ggplot(aes(x = `Temperature..K.`, y = 1, size = `Radius.R.Ro.`, color = color))+
  geom_jitter(set.seed(2021), height = 0.1, width = 1000)+
  scale_color_identity()+
  scale_x_continuous(position = "top",
                     limits = c(0, 42000),
                    # labels = scales::comma,
                     label = function(x){return(paste(x, "K"))})+
  
  annotate("text", label = "Stars", x = 3000, y = 0.8, color = "white", 
           hjust = 0, size = 10, family = "Patua")+
  annotate("text", 
           label = "github.com/kaizadp | source: Kaggle \n#30DayChartChallenge 2021: 12-strip", 
           # x = 25000, y = 0.8, color = "white", 
           x = 3000, y = 0.75, hjust = 0,
           color = "white", 
           size = 2.5, family = "Barlow")+
  
  annotate("text", label = "spectral class", 
           x = 19000, y = 0.78, color = "grey60", 
           hjust = 0, size = 3.5, family = "Barlow")+
  
  geom_point(data = star_label, 
             aes(x = x, color = color, y = 0.75), 
             size = 7)+
  geom_text(data = star_label, 
             aes(x = x, y = 0.75, label = SpectralClass), 
             size = 3.5, color = "black", fontface = "bold")+

  ylim(0.7, 1.15)+
  labs(x = "star temperature")+


  theme(plot.background = element_rect(fill = "black"),
        axis.text.x = element_text(color = "grey60", family = "Barlow",
                                   margin = margin(0, 0, -5, 0)),
        axis.title.x = element_text(color = "grey60", family = "Barlow",
                                   margin = margin(20, 0, -20, 0),
                                   hjust = 0.03, size = 10, face = "italic"),
        legend.position = "none")

ggsave("images/12-strip.png")
