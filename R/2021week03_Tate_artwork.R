# Tate artwork, TidyTuesday 2021, week 3
# By Emma Skarstein, October 2021


library(tidyverse)
library(tidytuesdayR)
library(showtext)
library(myGraphicsToolkit)

tuesdata <- tidytuesdayR::tt_load(2021, week = 3)
artwork <- tuesdata$artwork
artists <- tuesdata$artists

# Merge datasets
art_artist <- right_join(artists, artwork, by = c("name" = "artist")) %>%
  filter(year>1700)

# Initial test plot
test <- ggplot(art_artist, aes(x = year, fill = gender)) +
  geom_bar()
test

# Plot for anatomy-drawing
test + facet_grid(rows = vars(gender))
#ggsave("output/facet_tate_basic.pdf")

# Removing unknown gender
basic <- ggplot(art_artist %>% drop_na(gender), aes(x = year, fill = gender)) +
  geom_bar()

# theme_bw
basic + theme_bw()

# theme_minimal
basic + theme_minimal()

# my_basic_theme
basic + my_basic_theme()

# Specify this for font
font_add_google(name = "Nunito", family = "Nunito")
font_families()
showtext_auto()

basic +
  scale_fill_manual(values = c("#EC7238", "#66B4C0", "#1E4A76")) +
  scale_x_continuous(breaks = seq(1700, 2012, by = 50),
                     expand = c(0.05, 0.05)) +
  scale_y_continuous(limits = c(0, 3300),
                     expand = c(0.001, 0)) +
  labs(title = "TATE ART MUSEUM: Pieces created each year",
       subtitle = "1700 - 2012",
       caption = "Source: Tate Art Museum / TidyTuesday | Visualization: Emma Skarstein") +
  my_basic_theme(base_family = "Nunito")

ggsave("output/2021week03tate_final.pdf")
ggsave("output/2021week03.png")

# Todo:
# - Move legend to the top, fix legend title and labels
# - Remove axis lables
# - Remove gray background
# - Make flatter and longer
# - Highlight first female painter
# - Fix colors

