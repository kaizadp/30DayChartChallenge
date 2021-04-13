# 30DayChartChallenge
# Day 2: pictogram
# Kaizad F. Patel
# 2021-04-02

##

# -------------------------------------------------------------------------

devtools::install_github("hrbrmstr/waffle")

library(tidyverse)
library(waffle)

# run install_fa_fonts()
# then go to the location and install the fonts manually
# then run:
extrafont::font_import()
extrafont::loadfonts(quiet = TRUE)

extrafont::loadfonts("pdf")

maine_dat = 
tibble(
  type = c("person", "tree", "filler"),
  count = c(1, 167, 3)
  ) %>% 
  mutate(type = factor(type, levels = c("tree", "filler", "person")))


maine_dat = 
  tribble(
    ~type, ~count,
    "tree", 167.5,
    "filler", 3,
    "person", 1
  ) %>%   mutate(type = factor(type, levels = c("tree", "filler", "person")))

  
    maine_dat %>% 
    #  filter(type == "tree") %>% 
    ggplot(aes(values = count, label = type, color = type))+
    #geom_waffle(fill = "darkgreen", size = 0.5)+
    geom_pictogram(n_rows = 10,
                   size = 12 #, color = "darkgreen"
                   )+
    scale_label_pictogram(values = c(tree = "tree", filler = "apple", person = "male" ))+
      scale_color_manual(values = c("darkgreen", "black", "black"))+
    coord_equal()+
      labs(title = "Maine: The Pine Tree State",
           subtitle = "Maine has >16700 trees per person",
           caption =  "github.com/kaizadp | source: Northern Research Station (2007)
           #30DayChartChallenge 2021| 2: pictogram \n")+
    theme_void()+
      annotate("segment", x = 17.5, xend = 17.5, y = 0.5, yend = 11, size = 1)+
      annotate("text", label = "x100", x = 18, y = 11)+
      theme(legend.position = "none",
            plot.title = element_text(hjust = 0.5,
                                      family = "DINAlternate-Bold", size = 20),
            plot.subtitle = element_text(hjust = 0.5,
                                         family = "Arial Italic", color = "grey30"),
            plot.caption = element_text(hjust = 0.5, color = "grey50"),
            strip.text.x = element_text(hjust = 0.07),
          strip.text.y = element_text(angle = 90),
          plot.background = element_rect(fill = "#F9F8F1", color = NA))

ggsave("images/2-pictogram.png")


# -------------------------------------------------------------------------
## New England data

trees_dat = 
  tribble(
    ~state, ~trees, ~value, ~label,
    "Maine", 16751, 167, "Maine: 16751 trees per person",
    "New Hampshire", 2857, 30, "New Hampshire: 2857",
    "Vermont", 5504, 55, "Vermont: 5504",
    "Connecticut", 246, 24, "Connecticut: 246",
    "Massachussetts", 233, 2, "Massachussetts: 233",
    "Rhode Island", 171, 2, "Rhode Island: 171"
  ) %>% 
  mutate(label = fct_reorder(label, -trees)) %>% 
  mutate(state = factor(state, levels = c("Maine", "Vermont", "New Hampshire", "Connecticut", 
                                          "Massachussetts", "Rhode Island")))

# -------------------------------------------------------------------------

# FIA package

##  install.packages("rFIA")
##  library(rFIA)
##  library(tidyverse)
##  
##  northEast <- getFIA(states = c('ME', 'NH', 'VT', 'NY', 'CT', 'MA', 'RI'))
##  ct <- getFIA(states = 'CT')
##  
##  names(ct)
##  
##  
##  riMR <- clipFIA(fiaRI, mostRecent = TRUE)
##  tpaRI_MR <- tpa(riMR)
##  tpaRI_species <- tpa(riMR, bySpecies = TRUE)
##  ct_carbon = rFIA::carbon(ct)
##  
##  ct_carbon %>% 
##    ggplot(aes(x = YEAR, y = CARB_ACRE, fill = POOL)) + 
##    geom_bar(stat = "identity")
##  
##  
##  writeFIA(northEast, dir = "code/data")
##  ne_carbon = carbon(northEast, grpBy = STATECD)
##  
##  ne_carbon %>% 
##    filter(YEAR == 2019) %>% 
##    ggplot(aes(x = as.character(STATECD), y = CARB_ACRE, fill = POOL)) + 
##    geom_bar(stat = "identity")+
##    theme_classic()
