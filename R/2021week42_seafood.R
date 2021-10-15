# Seafood production, TidyTuesday 2021, week 42
# By Emma Skarstein, October 2021

library(tidyverse)
library(showtext)
library(myGraphicsToolkit)
library(ggimage)
library(gghighlight)


farmed <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-12/aquaculture-farmed-fish-production.csv')
captured_vs_farmed <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-12/capture-fisheries-vs-aquaculture.csv')
captured <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-12/capture-fishery-production.csv')
consumption <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-12/fish-and-seafood-consumption-per-capita.csv')
stock <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-12/fish-stocks-within-sustainable-levels.csv')
fishery <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-12/global-fishery-catch-by-sector.csv')
production <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-12/seafood-and-fish-production-thousand-tonnes.csv')

norway <- captured_vs_farmed %>% filter(Entity == "Norway") %>%
  rename(farmed = "Aquaculture production (metric tons)",
         captured = "Capture fisheries production (metric tons)") %>%
  #pivot_longer(cols = c(farmed, captured), names_to = "method", values_to = "tons") %>%
  mutate(farmed = farmed/1000, captured = captured/1000)
names(norway)

# Loading fonts
f2 <- "Josefin Sans"
f1 <- "Spectral SC"

font_add_google(name = f1, family = f1)
font_add_google(name = f2, family = f2)
showtext_auto()

# Setting colors
col_bg <- "#045174"
col_text <- "gray85"
col_cap_farm <- c("#D89C60", "#E87A00", "darkorange3")

ggplot(norway, aes(x = Year)) +
  # The data
  geom_area(aes(y = farmed + captured), fill = "darkgray", alpha = 0.45) +
  geom_area(aes(y = captured), color = "#E87A00", fill = "#E87A00", alpha = 0.9) +
  geom_area(aes(y = farmed), color = "#D89C60", fill = "#D89C60", alpha = 0.8) +
  # Annotations
  annotate("text", x = 2017, y = c(500, 1800, 3070), label = c("farmed", "captured", "total"),
           family = f2, hjust = "right", vjust = "bottom", color = col_text, fontface = "bold", size = 8) +
  # Scale & stuff
  scale_x_continuous(limits = c(1960, 2018), expand = c(0.005, 0.005))+
  ylab("thousand tons") +
  # Titles
  labs(title = "Seafood production \nand capture in Norway",
       subtitle = "1960 - 2018",
       caption = "Source: OurWorldinData.org  |  Visualization: Emma Skarstein") +
  theme(text = element_text(family = f2, color = col_text, size = 18),
        plot.title = element_text(family = f1,
                                  size = 40,
                                  face = "bold",
                                  margin = margin(b = 5)),
        plot.subtitle = element_text(size = 25,
                                     margin = margin(b = 10)),
        plot.caption = element_text(size = 12, margin = margin(t = 15)),
        axis.title.x = element_blank(),
        axis.text = element_text(color = col_text),
        axis.ticks = element_blank(),
        axis.text.x = element_text(margin = margin(t = -6)),
        # Background
        plot.background = element_rect(fill = col_bg, color = col_bg),
        panel.background = element_rect(fill = col_bg, color = col_bg),
        panel.grid = element_line(color = "gray60"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.background = element_rect(fill = col_bg, color = col_bg),
        plot.margin = margin(20, 20, 20, 20),
        legend.position = "top",
        legend.title = element_blank())


ggsave("output/2021week42_seafood_dark.pdf", width = 11, height = 8)
ggsave("output/2021week42_seafood_dark.png", width = 11, height = 8)



# SECOND PLOT: Make it look like the ocean?

# Setting colors
col_bg <- "#e1e7e0"
col_text <- "#2b4560"
col_farm <- "#2f6d80"
col_cap <- "#6aa4b0"

ggplot(norway, aes(x = Year)) +
  # The data
  geom_area(aes(y = farmed + captured), fill = "darkgray", alpha = 0.45) +
  geom_area(aes(y = captured), color = col_cap, fill = col_cap, alpha = 0.9) +
  geom_area(aes(y = farmed), color = col_farm, fill = col_farm, alpha = 0.8) +
  # Annotations
  annotate("text", x = 2017, y = c(500, 1800, 3070), label = c("farmed", "captured", "total"),
           family = f2, hjust = "right", vjust = "bottom", color = col_text, fontface = "bold", size = 8) +
  # Scale & stuff
  scale_x_continuous(limits = c(1960, 2018), expand = c(0.005, 0.005))+
  ylab("thousand tons") +
  # Fish
  #geom_image(aes(x = 1975, y = 1500), image = "R/icons/fish.svg", size = 0.25) +
  # Titles
  labs(title = "Seafood production \nand capture in Norway",
       subtitle = "1960 - 2018",
       caption = "Source: OurWorldinData.org  |  Visualization: Emma Skarstein") +
  theme(text = element_text(family = f2, color = col_text, size = 18),
        plot.title = element_text(family = f2,
                                  size = 47,
                                  face = "bold",
                                  margin = margin(b = 5)),
        plot.subtitle = element_text(size = 25,
                                     margin = margin(b = 10)),
        plot.caption = element_text(size = 12, margin = margin(t = 15)),
        axis.title.x = element_blank(),
        axis.text = element_text(color = col_text),
        axis.ticks = element_blank(),
        axis.text.x = element_text(margin = margin(t = -6)),
        # Background
        plot.background = element_rect(fill = col_bg, color = col_bg),
        panel.background = element_rect(fill = col_bg, color = col_bg),
        panel.grid = element_line(color = "gray60"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.background = element_rect(fill = col_bg, color = col_bg),
        plot.margin = margin(20, 20, 20, 20),
        legend.position = "top",
        legend.title = element_blank())

ggsave("output/2021week42_seafood_light.pdf", width = 11, height = 8)
ggsave("output/2021week42_seafood_light.png", width = 11, height = 8)



# THIRD PLOT: Seafood consumption in different countries
names(consumption)
names(consumption) <- c("country", "code", "year", "consumption")
library(RColorBrewer)

# Setting colors
col_bg <- "grey94"
col_text <- "#2b4560"

ggplot(consumption, aes(x = year, y = consumption, group = country)) +
  geom_line(aes(color = country)) +
  gghighlight(max(consumption) > 76 || country %in% c("Norway", "Sweden", "Japan"),
              use_direct_label = FALSE) +
  ylab("Seafood consumption (kg/capita/year)") +
  scale_color_brewer(palette = "Dark2", direction = -1) +
  scale_x_continuous(limits = c(1960, 2018), expand = c(0.005, 0.005)) +
  # Titles
  labs(title = "Global seafood consumption",
       subtitle = "1960 - 2018, selected countries highlighted",
       caption = "Source: OurWorldinData.org  |  Visualization: Emma Skarstein") +
  # Theme
  theme(text = element_text(family = f2, color = col_text, size = 18),
        plot.title = element_text(family = f2,
                                  size = 40,
                                  face = "bold",
                                  margin = margin(b = 5)),
        plot.subtitle = element_text(size = 25,
                                     margin = margin(b = 10)),
        plot.caption = element_text(size = 12, margin = margin(t = 15)),
        axis.title.x = element_blank(),
        axis.text = element_text(color = col_text),
        axis.ticks = element_blank(),
        axis.text.x = element_text(margin = margin(t = -6)),
        # Background
        plot.background = element_rect(fill = col_bg, color = col_bg),
        panel.background = element_rect(fill = col_bg, color = col_bg),
        panel.grid = element_line(color = "gray60"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        plot.margin = margin(20, 20, 20, 20),
        #legend.position = "top",
        legend.title = element_blank(),
        legend.key = element_rect(color = col_bg, fill = col_bg),
        legend.background = element_rect(fill = col_bg, color = col_bg))

ggsave("output/2021week42_seafood_consumption.pdf", width = 11, height = 8)
ggsave("output/2021week42_seafood_consumption.png", width = 11, height = 8)

