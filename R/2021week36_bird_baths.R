# Bird baths, TidyTuesday 2021, week 36
# By Emma Skarstein, October 2021

library(tidyverse)
library(showtext)
library(ggimage)
library(ggrepel)

bird_baths <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-08-31/bird_baths.csv')

names(bird_baths)

birds <- bird_baths %>% filter(bird_count > 0) %>% drop_na(urban_rural) %>%
  count(bird_type, urban_rural)

top_urban <- birds %>% arrange(n) %>% filter(urban_rural == "Urban")
top_rural <- birds %>% arrange(n) %>% filter(urban_rural == "Rural")
# Note that the diversity seems to be about the same in urban and rural locations

top_birds <- birds %>% filter(n > 20)

plot_data <- top_birds %>%
  pivot_wider(id_cols = bird_type, names_from = urban_rural, values_from = n) %>%
  drop_na(Urban) %>% drop_na(Rural)


# Idea: look at most common birds by location
# urban/rural location

ggplot(data = top_birds, aes(x = n, y = bird_type, fill = urban_rural)) +
  geom_col()


ggplot(top_birds, aes(x = rnorm(nrow(top_birds)), y = rnorm(nrow(top_birds)))) +
  geom_point(aes(size = n, color = urban_rural))

# Loading fonts
f1 <- "EB Garamond"
f2 <- "Satisfy"

font_add_google(name = f1, family = f1)
font_add_google(name = f2, family = f2)
showtext_auto()

# Setting colors
col_bg <- "gray96"
col_pointline <- "darkorange3"
col_catbird <- "gray30"


ggplot(data = plot_data) +
  # Years axis
  annotate("segment", x = 1.05, xend = 1.95,
           y = seq(10, 240, by = 10),
           yend = seq(10, 240, by = 10),
           alpha = 0.2, size = 0.2) +
  annotate("point", x = 1.5,
           y = seq(20, 240, by = 20), size = 16, color = col_bg) +
  annotate("text", x = 1.5,
           y = seq(20, 240, by = 20),
           label = seq(20, 240, by = 20),
           size = 8, alpha = 0.2, family = f1, fontface = "bold", color = "#41115B") +
  # Urban/rural annotation
  annotate("text", x = c(0.99, 2.01), y = 250, label = c("Rural", "Urban"),
           hjust = c(1, 0), family = f2, fontface = "bold", size = 10,
           alpha = 0.9, color = "#41115B") +
  # Lines
  geom_segment(aes(y = Rural, yend = Urban, x = 1, xend = 2),
               col = col_pointline, alpha = 0.5) +
  # Points
  geom_point(aes(y = Rural, x = 1), size = 2, col = col_pointline, alpha = 0.5) +
  geom_point(aes(y = Urban, x = 2), size = 2, col = col_pointline, alpha = 0.5) +
  # Bird names annotation
  geom_text_repel(aes(label=bird_type, x = 2, y = Urban), family = f1,
                  size = 2, nudge_x = 0.05, hjust = "left",
                  segment.color = col_catbird, segment.size = 0.1) +

  # Bird and cat
  geom_image(aes(x = 0.995, y = max(Rural)+5), image = "R/bird.svg", color = col_catbird) +
  geom_image(aes(x = 2.005, y = 200), image = "R/bird-flip.svg", color = col_catbird) +
  geom_image(aes(x = 0.8, y = 37), image = "R/cat.svg", size = 0.15, color = col_catbird) +
  # Scales
  scale_x_continuous(limits = c(0.7, 2.3)) +
  # Titles
  labs(title = "Avian Assemblages at Bird Baths",
       subtitle = "Bird counts for the 30 most commonly recorded bird species. Data is collected by citizen scientists \nin 2014 and 2015 in different regions of South-Eastern Australia.",
       caption = "Source: G.P. Cleary, H. Parsons, A. Davis, B.R. Coleman, D.N. Jones, K.K. Miller, M.A. Weston (2016)  |  Visualization: Emma Skarstein") +
  theme_void() +
  theme(text = element_text(family = f1),
        plot.title = element_text(family = f2,
                                  size = 48,
                                  margin = margin(b = 5)),
        plot.subtitle = element_text(size = 15,
                                     margin = margin(b = 10)),
        plot.caption = element_text(size = 10),
        plot.background = element_rect(fill = col_bg, color = col_bg),
        panel.background = element_rect(fill = col_bg, color = col_bg),
        plot.margin = margin(20, 20, 20, 20))


ggsave("output/2021week36_birdbath.pdf", width = 9, height = 11)
ggsave("output/2021week36.png", width = 9, height = 11)






