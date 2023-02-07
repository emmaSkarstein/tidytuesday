# Arthistory, TidyTuesday 2023, week 3
# By Emma Skarstein, January 2023

library(tidyverse)
library(janitor)
library(showtext)
library(ggtext)

showtext_auto()
showtext_opts(dpi = 300)

artists <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-01-17/artists.csv')

page_height <- 1.3

space_by_race_gender_2020 <- artists %>%
  filter(year == 2020, artist_gender != "N/A") %>%
  group_by(artist_race_nwi, artist_gender) %>%
  summarise(mean_space = mean(space_ratio_per_page_total),
            n = n()) %>%
  ungroup() %>%
  mutate(artist_plural = ifelse(n > 1, "artists", "artist"),
         area_on_page = mean_space*page_height,
         side_lengths = sqrt(area_on_page))

f1 <- "Cabin" # Title font
f2 <- "Cabin" # Body text font
font_add_google(name = f1, family = f1)
font_add_google(name = f2, family = f2)

# Colors
col_bgr <- "#F0F0F0"
col_text <- "#191919"
col_page <- "#ECEBE4"

page_margin <- 0.08

ggplot(space_by_race_gender_2020, aes(x = 0, y = 0)) +
  geom_rect(aes(xmin = page_margin, xmax = side_lengths + page_margin,
                ymin = page_margin, ymax = side_lengths + page_margin),
            fill = col_text) +
  geom_text(aes(label = paste0("(", n, " ", artist_plural, ")"),
                x = (side_lengths + page_margin)/2,
                y = side_lengths + 1.5*page_margin + 0.01),
            family = f2, color = col_text) +
  geom_text(aes(label = paste0(round(mean_space*100), "% of page"),
                x = (side_lengths + page_margin)/2,
                y = side_lengths/2 + 1.5*page_margin),
            family = f1, color = col_page) +
  facet_grid(rows = vars(artist_race_nwi), cols = vars(artist_gender)) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  coord_fixed(ylim = c(0, page_height), xlim = c(0, 1)) +
  labs(title = "White space: Whose art takes up space \nin art history books?",
       subtitle = "The squares show the average area occupied by pieces of art in <br>Gardner’s *Art Through the Ages* (16th edition, published in 2020), <br>according to the artist's gender and whether or not they are white. <br>Note also the number of artists in each of the groups.",
       caption = "Source: Holland Stam, 'Quantifying art historical narratives' | Graphics: Emma Skarstein"
       ) +
  theme(text = element_text(family = f2, color = col_text),
        plot.title = element_text(family = f1, face = "bold",
                                  size = 18, margin = margin(t = 10, b = 16)),
        plot.subtitle = element_markdown(size = 10.5,
                                     margin = margin(b = 10)),
        plot.caption = element_text(size = 8,
                                    margin = margin(t = 10)),
        strip.text = element_text(size = 12, face = "bold"),
        #strip.background = element_rect(fill = "grey97"),
        strip.background = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        panel.background = element_rect(fill = col_page),
        plot.margin = margin(20, 20, 20, 20))


ggsave("output/2023week03_arthistory.pdf", height = 8, width = 5.5)
ggsave("output/2023week03_arthistory.png", height = 8, width = 5.5)

