# 30DayChartChallenge
# Day 4: magic
# Kaizad F. Patel
# 2021-04-04

## creating a packed circle plot masked within the Flame of Tar Valon 
## code adapted from: https://www.r-bloggers.com/2018/12/bubble-packed-chart-with-r-using-packcircles-package/
## data taken from: https://library.tarvalon.net/index.php?title=Aes_Sedai_Character_List

# -------------------------------------------------------------------------
# load packages and fonts
library(imager) ## to create data frame from image
library(scales) 

sysfonts::font_add_google("Patua One", "Patua")
sysfonts::font_add_google("Lato", "Lato")
sysfonts::font_add_google("Fondamento", "Fondamento")
sysfonts::font_add_google("Lora", "Lora")
showtext::showtext_auto()

# step 1: clean the data ----
## data saved from the website as .txt file
## extract names and Ajahs
## Manually fix some of the Ajahs, because they were messed up.
## Manually set values for "importance". This is randomly chosen by me. 

aes_sedai = read_tsv("data/4-aes_sedai.txt") %>% as_tibble()
aes_sedai_cleaned = 
  aes_sedai %>% 
  separate(aes_sedai, sep = ": ", into = c("name", "info")) %>% 
  filter(!grepl("male", info)) %>% 
  mutate(Yellow = str_extract(info, "Yellow"),
         Red = str_extract(info, "Red"),
         Blue = str_extract(info, "Blue"),
         Green = str_extract(info, "Green"),
         White = str_extract(info, "White"),
         Gray = str_extract(info, "Gray"),
         Brown = str_extract(info, "Brown"),
         Black = str_extract(info, "Black"),
         ajah = paste0(Yellow, Red, Blue, Green, White, Gray, Brown, Black),
         ajah = str_remove_all(ajah, "NA"),
         ajah = if_else(grepl("Black", ajah), "Black", ajah),
         ajah = if_else(grepl("Yellow", ajah), "Yellow", ajah),
         ajah = if_else(ajah == "", "unknown", ajah),
         ajah = if_else(grepl("Beonin", name), "Gray", ajah),
         ajah = if_else(grepl("Bernaile", name), "White", ajah),
         ajah = if_else(grepl("Jennet", name), "Gray", ajah),
         ajah = if_else(grepl("Meidani", name), "Gray", ajah),
         ajah = if_else(grepl("Siuan", name), "Blue", ajah),
         ajah = if_else(grepl("Teslyn", name), "Red", ajah),
         ajah = if_else(grepl("Zerah", name), "White", ajah),
         ajah = if_else(grepl("Leane", name), "Green", ajah),
         ajah = if_else(grepl("Teramina", name), "Green", ajah),
         ajah = if_else(grepl("Laigin", name), "Red", ajah),
         ajah = if_else(grepl("Egwene", name), "Green", ajah),
         ajah = if_else(grepl("Verin", name), "Brown", ajah),
         
         ) %>% 
  # set values
  mutate(value = case_when(
    grepl("Moiraine", name) ~ 1,
    grepl("Egwene", name) ~ 3,
    grepl("Nynaeve", name) ~ 2,
    grepl("Elayne", name) ~ 4,
    grepl("Siuan", name) ~ 5,
    grepl("Cadsuane", name) ~ 6,
    grepl("Verin", name) ~ 7,
    grepl("Elaida", name) ~ 8,
    
    grepl("Sheriam", name) ~ 9,
    grepl("Alviarin", name) ~ 10,
    grepl("Liandrin", name) ~ 11,
    
    grepl("Alanna", name) ~ 12,
    grepl("Leane", name) ~ 13,
    grepl("Silviana", name) ~ 14,
    
    grepl("Morgase", name) ~ 15,
    grepl("Lelaine", name) ~ 16,
    grepl("Romanda", name) ~ 17,
    grepl("Myrelle", name) ~ 18,
    grepl("Nicola", name) ~ 19,
    grepl("Theodrin", name) ~ 20,
    grepl("Delana", name) ~ 21,
    grepl("Sareitha", name) ~ 22,
    grepl("Therava", name) ~ 23)) %>% 
  
  mutate(value = as.integer(value),
         value2 = sample(40:2000, n(), replace = FALSE),
         id = if_else(!is.na(value), value, value2)) %>% 
  dplyr::select(name, ajah, info, id) %>% 
  force()


# step 2: import image for masking ----

im <- load.image("data/yin.png") #memoji2
#plot(im)

## Convert Image into Data Frame
im.df.colour <- im %>%
  as.data.frame(wide="c") %>% ## so that rgb value is in separate column.
  rename(im_x=x,im_y=y) %>%
  mutate(hex=rgb(c.1,c.2,c.3)) %>% 
  # the dataframe includes the black and white zones, so remove the white.
  filter(hex != "#FFFFFF") %>% 
  force()

## Generate circle packing layout using rbeta distribution as size of circles
## the rbeta is randomly generated, so commented out for now.
## the output is saved, in case it needs to be used for future work.

## pack_layout_base <- circleProgressiveLayout(c(rbeta(5000, 1,30), 0.6,0.6,0.6), sizetype='radius') %>% 
##   ## Step 3 - I want to figure out what colour to use, so I want layout & image df to have same scaling. 
##   mutate(im_x=floor(rescale(x,to=range(im.df.colour$im_x))),  
##          im_y=floor(rescale(y,to=range(im.df.colour$im_y))),
##          ## also generate id, so i can join the data frame easily later!
##          id=row_number()) %>% 
##   inner_join(im.df.colour %>% select(im_x,im_y,hex), by=c("im_x","im_y"))

write.csv(pack_layout_base, "code/4-aes_sedai_pack_layout_base.csv", row.names = FALSE)

#
# step 3: more processing ----

## jerry-rigging the code
## arrange the circles by descending radius, and then link to the Aes Sedai dataset
## assign colors

pack_layout2 = 
  pack_layout_base %>% 
  arrange(-radius) %>% 
  dplyr::select(-id) %>% 
  mutate(id = rownames(.)) %>% 
  mutate(id = as.integer(id)) %>% 
  left_join(aes_sedai_cleaned) %>% 
  mutate(ajah = if_else(is.na(ajah), "NA", ajah)) %>% 
  mutate(color = recode(ajah,
                        "Black" = "#000000",
                        "Blue" = "#118ab2",
                        "Brown" = "#a44a3f",
                        "Gray" = "#8d99ae",
                        "Green" = "#2a9d8f",
                        "Red" = "#e63946",
                        "unknown" = "#fdf0d5",
                        "NA" = "#fdf0d5",
                        "White" = "#FFFFFF",
                        "Yellow" = "#ffbe0b"
                        ))

## Using the layout above create data frame using circleLayoutVertices function so that you can plot circle using ggplot2
data_gg_base <- circleLayoutVertices(pack_layout2) %>% 
  inner_join(pack_layout2 %>% dplyr::select(id, name, ajah, color), 
             by=c("id"))


#

# step 4: make the plot ----

## make label files

label = data_gg_base %>% 
  group_by(id, name) %>% 
  dplyr::summarise(x = mean(x), y = mean(y))

label2 = 
  label %>% 
  filter(id <= 5) %>% 
  mutate(lab = paste0(id, ". ", name, "\n")) %>%  pull(lab)

label3 = 
  label %>% 
  filter(id <= 10 & id > 5) %>% 
  mutate(lab = paste0(id, ". ", name, "\n")) %>%  pull(lab)

## make the plot

data_gg_base %>% 
  ggplot(aes(x=x,y=y,group=id)) +
  
  geom_polygon(aes(fill = color), color = "black", alpha = 1) +  
  geom_text(data = label %>% filter(id <= 10), 
            aes(label = id), color = "white", size = 4,
            family = "Lato", fontface = "bold")+
  
  annotate("text", label = paste(label2, collapse=""), 
           x = 3.5, y = -3.1, hjust = 0, vjust = 1,
           size = 3.3, fontface = "italic", color = "grey30", family = "Lora")+
  annotate("text", label = paste(label3, collapse=""), x = -4, y = 3.5, hjust = 1, vjust = 0,
           size = 3.3, fontface = "italic", color = "grey30", family = "Lora")+
  
  annotate("text", label = "The Aes Sedai", x = -1.7, y = -2.7, 
           fontface = "bold", family = "Fondamento", size = 14)+
  annotate("text", label = "from The Wheel of Time", x = -1.7, y = -2.2,
           fontface = "italic", family = "Lora", size = 6)+
  
  annotate("text", label = "Known Aes Sedai are represented by their Ajah colors. \nSizes represent relative importance in the WOT storyline.", 
           x = -1.7, y = -1.5,
           family = "Lato", size = 3.5, color = "grey10")+
  
  annotate("text", label = "github.com/kaizadp \nsource: The Wheel of Time, TarValon.Net \n#30DayChartChallenge 2021 | 4: magical\n", 
           x = -1.7, y = -0.5, family = "Lato", size = 3,
           color = "grey40")+
  
  scale_fill_identity() + 
  coord_equal() +
  scale_y_reverse() +  ## you need to reverse y-axis
  scale_x_reverse() +  ## you need to reverse y-axis
  theme_void()+
  theme(plot.background = element_rect(fill = "#F4F6F3", color = NA))+
  NULL

ggsave("images/4-magical.png", width = 10, height = 8)
