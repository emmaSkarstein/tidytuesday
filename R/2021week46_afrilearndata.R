# afrilearndata, TidyTuesday 2021, week 46
# By Emma Skarstein, November 2021

# Get the Data

# Data accessed via the afrimapr or afrilearndata packages

# afrilearndata
remotes::install_github("afrimapr/afrilearndata")
# afrihealthsites`
remotes::install_github("afrimapr/afrihealthsites")

library(afrilearndata)
library(afrihealthsites)
library(sf)
library(raster)
library(tidyverse)
library(ggthemes) # theme_map
library(paletteer)
library(showtext)
library(ggtext) #element_markdown

pal <- paletteer_d("NineteenEightyR::miami1")

head(afrilearndata::africountries$geometry)
afrilearndata::africountries$geometry

countries <- afrilearndata::africountries
airports <- afrilearndata::afriairports %>% mutate(type_num = (4-as.numeric(as.factor(type))))
capitals <- afrilearndata::africapitals
highways <- afrilearndata::afrihighway
pop2000 <- afrilearndata::afripop2000
pop2020 <- afrilearndata::afripop2020
healthsites <- afrihealthsites::df_who_sites

# Font
f1 <- "Staatliches"
f2 <- "News Cycle"
font_add_google(name = f1, family = f1)
font_add_google(name = f2, family = f2)
showtext_auto()

# Colors
col_bg <- "#EEF8F7"

ggplot() +
  geom_sf(data = highways, color = pal[2]) +
  geom_sf(data = airports, size = 1.5, color = "black", stroke = 0, alpha = 0.3) +
  geom_sf(data = capitals, color = pal[4], size = 3, stroke = 0, alpha = 0.6) +
  labs(title = "African Airports",
       subtitle = "",
       caption = "Source: afrimapr team  |  Visualization: Emma Skarstein") +
  theme_map(base_family = f2) +
  theme(text = element_text(family = f2, size = 18),
        plot.title = element_text(family = f1,
                                  size = 60,
                                  face = "bold",
                                  hjust = 0.5,
                                  margin = margin(b = 5)),
        plot.subtitle = element_markdown(size = 15,
                                         margin = margin(b = 10)),
        plot.caption = element_text(size = 8,
                                    margin = margin(t = 40)),
        panel.background = element_rect(fill = col_bg, color = col_bg),
        plot.background = element_rect(fill = col_bg, color = col_bg),
        plot.margin = margin(20, 20, 0, 20))

ggsave("output/2021week46_afrilearndata_light.pdf", width = 8, height = 8)
ggsave("output/2021week46_afrilearndata_light.png", width = 8, height = 8)

# Dark version

# Colors
col_bg <- "gray5"

ggplot() +
  geom_sf(data = airports, size = 2, color = "#F4AC00", stroke = 0, alpha = 0.3) +
  geom_sf(data = highways, color = "#2136B3") +
  geom_sf(data = capitals, color = "#2136B3", size = 4.5, stroke = 0, alpha = 0.6) +
  labs(title = "African Airports",
       subtitle = "<b style='color:#F4AC00;'>AIRPORTS \t\t\t</b>
       <b style='color:#2136B3;'>   CAPITALS AND MAJOR ROADS</b>",
       caption = "SOURCE: AFRIMAPR TEAM  |  VISUALIZATION: EMMA SKARSTEIN") +
  theme_map(base_family = f2) +
  theme(text = element_text(family = f2, size = 18),
        plot.title = element_text(family = f1,
                                  size = 75,
                                  face = "bold",
                                  hjust = 0.5,
                                  color = "#F49500",
                                  margin = margin(t = 10, b = 40)),
        plot.subtitle = element_markdown(size = 20,
                                         hjust = 0.5,
                                         margin = margin(b = 10)),
        plot.caption = element_text(size = 10,
                                    color = "#F49500",
                                    margin = margin(t = 40)),
        panel.background = element_rect(fill = col_bg, color = col_bg),
        plot.background = element_rect(fill = col_bg, color = col_bg),
        plot.margin = margin(60, 20, 0, 20))

ggsave("output/2021week46_afrilearndata_dark.pdf", width = 9, height = 11)
ggsave("output/2021week46_afrilearndata_dark.png", width = 9, height = 11)


# Colors
col_bg <- "gray98"

ggplot() +
  geom_sf(data = highways, color = "gray30") +
  geom_point(data = healthsites, aes(x = Long, y = Lat), size = 0.1, color = "red", alpha = 0.25) +
  geom_sf(data = capitals, color = "gray30", size = 2, stroke = 0, alpha = 0.5) +
  labs(title = "African Healthsites",
       subtitle = "",
       caption = "Source: afrimapr team  |  Visualization: Emma Skarstein") +
  theme_map(base_family = f2) +
  theme(text = element_text(family = f2, size = 18),
        plot.title = element_text(family = f1,
                                  size = 60,
                                  face = "bold",
                                  hjust = 0.5,
                                  margin = margin(b = 5)),
        plot.subtitle = element_markdown(size = 15,
                                         margin = margin(b = 10)),
        plot.caption = element_text(size = 8,
                                    margin = margin(t = 40)),
        panel.background = element_rect(fill = col_bg, color = col_bg),
        plot.background = element_rect(fill = col_bg, color = col_bg),
        plot.margin = margin(20, 20, 0, 20))

ggsave("output/2021week46_afrilearndata_healthsites.pdf", width = 8, height = 8)
ggsave("output/2021week46_afrilearndata_healthsites.png", width = 8, height = 8)
