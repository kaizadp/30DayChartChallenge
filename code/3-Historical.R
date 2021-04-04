# 30DayChartChallenge
# Day 3: historical
# Kaizad F. Patel
# 2021-04-03

##
library(tidyverse)
library(ggforce)
theme_set(theme_void())

sysfonts::font_add_google("Patua One", "Patua")
sysfonts::font_add_google("Lato", "Lato")
showtext::showtext_auto()

# create a tibble with Middle Earth landmarks
# source: http://lotrproject.com/timeline/

lotr_dat = 
  tribble(
    ~age, ~year, ~event,
    "Years of the Trees \nYT", 1050, "Elves awaken",
    "YT", 1100, "Melkor imprisoned",
    "YT", 1362, "Galadriel born",
    "YT", 1450, "Silmarils created",
    "YT", 1500, "Moon and Sun created",
    "First Age \nFA", 001, "Men awaken",
    #    "FA", 457, "Sauron captures Minas Tirith",
    #    "FA", 500, "Fall of Gondolin",
    "FA", 532, "Elrond born",
    "FA", 590, "Morgoth cast into the Void",
    "Second Age \nSA", 32, "First king of Númenor",
       "SA", 500, "Sauron rises again",
    "SA", 1500, "Rings of Power constructed",
    "SA", 1600, "One Ring forged",
    "SA", 1693, "Rivendell created",
    "SA", 1700, "Sauron defeated #1",
    "SA", 2221, "Ringwraiths first appear",
    "SA", 3119, "Birth of Elendil",
    "SA", 3319, "Downfall of Númenor",
    #    "SA", 3429, "Sauron takes Minas Ithil",
    #    "SA", 3429, "White Tree burned",
    "SA", 3441, "Sauron defeated #2",
    "Third Age \nTA", 2, "One Ring lost",
    "TA", 1000, "The Istari arrive in Middle Earth",
    "TA", 1601, "The Shire created",
    "TA", 2002, "Minas Ithil falls",
    # "TA", 2460, "Sauron returns to Middle Earth",
    #    "TA", 2890, "Bilbo Baggins born",
    "TA", 2931, "Aragorn born",
    "TA", 2941, "Bilbo finds the One Ring",
    #    "TA", 2968, "Frodo Baggins born",
    #    "TA", 3018, "The Fellowship of the Ring",
    #    "TA", 3019, "Battle of the Hornburg",
    #    "TA", 3019, "The Ring is destroyed",
    "TA", 3019, "Sauron defeated #3",
    #    "TA", 3021, "Third Age ends"
  ) %>% 
  mutate(position = case_when(grepl("YT", age) ~ year,
                              grepl("FA", age) ~ year + 1500,
                              grepl("SA", age) ~ year + 1500 + 587,
                              grepl("TA", age) ~ year + 1500 + 587 + 3441),
         label = paste0(age, " ", year, "\n", event))



lotr_dat %>% 
  ggplot(aes(x = position, label = label, fill = age)) +

  annotate("segment", x = 950, xend = 1500, y = 0, yend = 0, size = 1, color = "#2a9d8f")+
  annotate("segment", x = 1500, xend = 2087, y = 0, yend = 0, size = 1, color = "#e9c46a")+
  annotate("segment", x = 2087, xend = 5528, y = 0, yend = 0, size = 1, color = "#219ebc")+
  annotate("segment", x = 5528, xend = 8549, y = 0, yend = 0, size = 1, color = "#e76f51")+
  
  geom_mark_circle(aes(y = 0, group = event, label = label, color = event), 
                   con.cap = -7, fill = NA, color = NA, label.fontsize = 8, label.hjust = 0,
                   label.fill = NA, label.family = "Lato",
                   con.size = 0.5, con.border = "one",
                   con.type = "elbow")+
  geom_point(aes(y = 0), size = 3, shape = 21, stroke = 1.5, fill = "white")+
  labs(title = "A History of Middle Earth",
       caption = "github.com/kaizadp | source: LOTR Project \n 
       #30DayChartChallenge | 3: historical \n")+
  #  theme_bw()+
  theme(legend.position = "none",
        plot.background = element_rect(fill = "#F9F8F1", color = NA),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 25, family = "Patua"),
        plot.subtitle = element_text(hjust = 0.5, face = "italic", size = 22),
        plot.caption = element_text(hjust = 0.5, color = "grey50", family = "Lato")) +
  xlim(950, 8700)+ ylim(-10,10)


ggsave("images/3-historical.png", width = 15, height = 7)
