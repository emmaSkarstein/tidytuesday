# Wastewater plants, TidyTuesday 2022, week 38
# By Emma Skarstein, September 2022


library(tidyverse)  # Data cleaning tools and ggplot2
library(showtext)   # Fonts
library(janitor)    # Cleaning names
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggmap) # For getting a higher resolution map of Trondheim
library(ggrepel) # Text overlapping

sf_use_s2(FALSE)


waste <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-09-20/HydroWASTE_v10.csv') %>%
  janitor::clean_names()

waste$country %>% unique()

norwaste <- waste %>% filter(country == "Norway")

# Map of Norway with wastewater stations plotted, size indicating pop_served

switzerland <- ggplot2::map_data("world", region = "Switzerland")

world <- ne_countries(scale = "medium", returnclass = "sf")
norway <- ne_countries(scale = 10, country = "Norway", returnclass = "sf")

buffer <- 2
lon_lims <- c(min(norwaste$lon_wwtp)-buffer, max(norwaste$lon_wwtp)+4+buffer)
lat_lims <- c(min(norwaste$lat_wwtp)+1.5-buffer, max(norwaste$lat_wwtp)+buffer)

ggplot(waste) +
  geom_sf(data = world) +
  coord_sf(expand = FALSE) +
  geom_point(aes(x = lon_wwtp, y = lat_wwtp, size = pop_served),
             color = "darkorange2", alpha = 0.3) +
  theme_bw()

ggplot(norwaste) +
  geom_sf(data = world) +
  coord_sf(xlim = lon_lims,
           ylim = lat_lims,
           expand = FALSE) +
  geom_point(aes(x = lon_wwtp, y = lat_wwtp, size = pop_served),
             color = "darkorange2", alpha = 0.3) +
  theme_bw()


trondlon <- c(9, 12)
trondlat <- c(63, 64)

trond_loc = c(10.1, 63.34, 10.5, 63.46)
trond_loc <- c(9.5, 63, 11.5, 64)
trond_map <- get_map(location=trond_loc,
                     source="stamen", maptype="watercolor", crop=FALSE)

ggmap(trond_map) +
  geom_point(data = norwaste,
             aes(x = lon_wwtp, y = lat_wwtp, size = waste_dis, color = status),
             alpha = 0.6) +
  geom_point(data = norwaste,
             aes(x = lon_wwtp, y = lat_wwtp),
             alpha = 0.6, size = 0.2) +
  geom_text_repel(data = norwaste,
            aes(x = lon_wwtp, y = lat_wwtp, label = wwtp_name),
            size = 3)



