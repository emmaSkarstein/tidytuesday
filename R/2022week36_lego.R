# LEGO sets, TidyTuesday 2022, week 36
# By Emma Skarstein, August 2022

#remotes::install_github("hrbrmstr/ggchicklet")

library(tidyverse)  # Data cleaning tools and ggplot2
library(showtext)   # Fonts
library(janitor)    # Cleaning names
library(ggtext)     # element_markdown

inventories <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-09-06/inventories.csv.gz')
inventory_sets <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-09-06/inventory_sets.csv.gz')
sets <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-09-06/sets.csv.gz')
colors <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-09-06/colors.csv.gz')

colors <- colors %>%
  mutate(x = rep(1:7, 31),
         y = rep(1:31, 7),
         rgb = paste0("#", rgb))

col_bkg <- "#237841"
col_black <- "#191919"

knobs <- expand.grid(x = seq(1-1/3, 7+2/3, by = 1/3), y = seq(1-1/3, 31+2/3, by = 1/3))

ggplot(colors, aes(x = x, y = y)) +
  ggchicklet:::geom_rrect(xmin = 0, xmax = 8, ymin = 0, ymax = 32,
                          fill = col_bkg) +
  geom_tile(aes(fill = I(rgb)), width = 2/3, height = 2/3, color = "#191919") +
  geom_point(data = knobs, aes(x = x-0.18, y = y-0.18),
             size = 0.7, shape = 21,
             color = "#191919", fill = NA, stroke = 0.15) +
  coord_fixed() +
  theme(panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.title = element_blank())


ggsave("output/2022week36_lego.png")
ggsave("output/2022week36_lego.pdf")
