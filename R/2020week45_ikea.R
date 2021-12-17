# IKEA furniture, TidyTuesday 2020, week 45
# By Emma Skarstein, December 2021

library(tidyverse)
#library(ggforce)
library(stringr)
library(showtext)

ikea <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-11-03/ikea.csv')

# Colors
ikea_blue <- "#0051BA"
ikea_yellow <- "#FFDA1A"

# Fonts
f1 <- "Noto Sans"
f2 <- "Noto Sans"

font_add_google(name = f1, family = f1)
font_add_google(name = f2, family = f2)
showtext_auto()

ikea %>% filter(category %in% c("Beds", "Bookcases & shelving units", "Cabinets & cupboards",
                                "Chairs", "Children's furniture", "Sofas & armchairs",
                                "Sideboards, buffets & console tables", "Tables & desks",
                                "Wardrobes")) %>%
  ggplot() +
  geom_tile(aes(x = 0, y = 0, height = depth, width = width), alpha = 0, color = ikea_blue) +
  facet_wrap(~category) +
  scale_y_continuous(limits = c(-200, 200)) +
  labs(title = "IKEA furniture seen from above") +
  theme(aspect.ratio = 1,
        plot.background = element_rect(fill = ikea_yellow),
        panel.background = element_rect(fill = ikea_yellow),
        strip.background = element_rect(fill = ikea_yellow),
        panel.grid = element_blank(),
        )


remove_terms <- paste(c("Storage box", "Desk", "drawer", "shlvs"), collapse = '|')

ikea_beds <- ikea %>% filter(category == "Beds") %>%
  filter(!grepl(remove_terms, short_description)) %>%
  mutate(size = str_extract(short_description, "[:digit:]+x[:digit:]+")) %>%
  mutate(Width = as.numeric(sub("x.*", "", size)),
         Length = as.numeric(sub(".*x", "", size))) %>% drop_na(Width)

n <- nrow(ikea_beds)
n_rows <- 8

xy_grid <- expand_grid(x = c(1, seq(250, 250*(n_rows-1), by = 250)), y = seq(0, 230*12, by = 230))


ikea_beds <- ikea_beds %>% mutate(x = xy_grid$x[1:n] + rnorm(n, 0, 20),
                                  y = xy_grid$y[1:n] + rnorm(n, 0, 20))

ikea_beds$name[which(ikea_beds$name == "LYCKSELE LÖVÅS")] <- "LYCKSELE \nLÖVÅS"
ikea_beds$name[which(ikea_beds$name == "LYCKSELE HÅVET")] <- "LYCKSELE \nHÅVET"
ikea_beds$name[which(ikea_beds$name == "LYCKSELE MURBO")] <- "LYCKSELE \nMURBO"

ggplot(ikea_beds) +
  geom_tile(aes(x = x, y = y, height = Width, width = Length),
            color = ikea_blue, fill = ikea_blue) +
  geom_text(aes(x = x, y = y-Width*0.1, label = name, size = Length/10), color = ikea_yellow, fontface = "bold") +
  scale_size_continuous(range = c(0.5, 2.3)) +
  labs(title = "IKEA beds seen from above",
       caption = "Source: IKEA (through Kaggle) |  Visualization: Emma Skarstein") +
  coord_fixed() +
  theme(text = element_text(family = f2, size = 12),
        plot.title = element_text(family = f1,
                                  size = 35,
                                  face = "bold",
                                  margin = margin(b = 5),
                                  color = ikea_blue,
                                  hjust = 0.5),
        plot.caption = element_text(size = 8,
                                    face = "bold",
                                    color = ikea_blue,
                                    margin = margin(t = 10)),
        panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        legend.position = "none",
        plot.background = element_rect(fill = ikea_yellow, color = ikea_yellow),
        panel.background = element_rect(fill = ikea_yellow),
        plot.margin = margin(20, 5, 5, 5))

ggsave("output/2021week51_ikea.pdf", width = 7, height = 11)
ggsave("output/2021week51_ikea.png", width = 7, height = 11)
