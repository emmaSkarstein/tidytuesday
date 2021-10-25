# Animal rescues, TidyTuesday 2021, week 27
# By Emma Skarstein, October 2021

library(tidyverse)
library(showtext)
library(myGraphicsToolkit)
library(sf)
library(maptools)
library(ggtext)
library(wesanderson)
library(patchwork)


animal_rescues <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-06-29/animal_rescues.csv')

animal_rescues$animal_group_parent <- fct_collapse(animal_rescues$animal_group_parent, Cat = c("Cat", "cat"))
animal_rescues$special_service_type_category <- fct_recode(animal_rescues$special_service_type_category,
                                                           Height = "Animal rescue from height",
                                                           Below_ground = "Animal rescue from below ground",
                                                           Water = "Animal rescue from water",
                                                           Other = "Other animal assistance")

# Select animals of interest. What are the most common animals?
animal_counts <- animal_rescues %>% group_by(animal_group_parent) %>% count()


# Most common animals are cats and birds. Choose to look at cats,
# and remove 2021 observations since year is not over.
cats <- animal_rescues %>% filter(animal_group_parent == "Cat", cal_year < 2021)

yearly_service_counts <- cats %>% group_by(special_service_type_category) %>%
  count(cal_year) %>% pivot_wider(names_from = "special_service_type_category", values_from = "n")

# Map
london <- readShapePoly("R/spatialggplot/london_sport.shp")  # read in the shapefile
proj4string(london) <- CRS("+init=epsg:27700")

# Font
f1 <- "Oswald"
f2 <- "Open Sans"
font_add_google(name = f1, family = f1)
font_add_google(name = f2, family = f2)
showtext_auto()

# Colors
cat_pal5 <- '["e3b505","95190c","610345","107e7d"]'
col_points <- myGraphicsToolkit::CreateRPalette(cat_pal5, n = 4)
col_points <- wes_palette("Royal1")
col_bg <- "gray10"
col_panel <- "gray20"
col_text <- "gray70"
col_lines <- "gray30"


# Some scalars for the bar
x0 <- 498000
y0 <- 157000
scalen <- 130
width <- 2

ggplot(cats) +
  # Background map
  geom_polygon(data = london, aes(long, lat, group = group), fill = "grey30", color = "grey40") +
  # Points for animal rescues
  geom_point(aes(x = easting_rounded, y = northing_rounded, fill = special_service_type_category),
             pch = 21, color = "black") +
  scale_fill_manual(values = col_points) +
  #geom_point(data = yearly_service_counts, aes(x = 500000+5000*(special_service_number-1), y = 160000, size = n, color = special_service_type_category)) +
  # The bar that shows total numbers
  geom_segment(data = yearly_service_counts,
               aes(x = x0, xend = x0, y = y0, yend = y0 + scalen*Below_ground),
               size = width, color = col_points[1]) +
  geom_segment(data = yearly_service_counts,
               aes(x = x0, xend = x0, y = y0 + scalen*Below_ground, yend = y0 + scalen*(Below_ground + Height)),
               size = width, color = col_points[2]) +
  geom_segment(data = yearly_service_counts,
               aes(x = x0, xend = x0, y = y0 + scalen*(Below_ground + Height), yend = y0 + scalen*(Below_ground + Height + Water)),
               size = width, color = col_points[3]) +
  geom_segment(data = yearly_service_counts,
               aes(x = x0, xend = x0, y = y0 + scalen*(Below_ground + Height + Water), yend = y0 + scalen*(Below_ground + Height + Water + Other) ),
               size = width, color = col_points[4]) +
  geom_text(data = yearly_service_counts,
            aes(x = x0 + 2600, y = y0 + scalen*(Below_ground + Height + Water + Other) + 3800,
                label = Below_ground + Height + Water + Other), color = col_text,
            family = f2, face = "bold") +
  labs(title = "2020 MARKS ALL-TIME HIGH FOR CAT RESCUES IN LONDON",
       subtitle = "The maps show yearly cat rescues
       <b style='color:#899DA4;'>below ground</b>,
       <b style='color:#C93312;'>from a hight</b>,
       <b style='color:#FAEFD1;'>from water</b> or
       <b style='color:#DC863B;'>other</b>.",
       caption = "Source: London.gov  |  Visualization: Emma Skarstein") +
  facet_wrap(vars(cal_year)) +
  coord_equal() +
  my_basic_theme(base_family = f2) +
  theme(text = element_text(family = f2, size = 18, color = col_text),
        plot.title = element_markdown(family = f1,
                                  size = 26,
                                  face = "bold",
                                  margin = margin(b = 5)),
        plot.subtitle = element_markdown(size = 15,
                                     margin = margin(b = 10)),
        plot.caption = element_text(size = 12,
                                    margin = margin(t = 10)),
        strip.text = element_text(size = 15,
                                  face = "bold",
                                  color = col_bg),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        plot.background = element_rect(fill = col_bg, color = col_bg),
        panel.background = element_rect(fill = col_panel, color = col_panel),
        legend.position = "none",
        panel.spacing = unit(.05, "lines"),
        panel.border = element_rect(color = col_lines, fill = NA, size = 1),
        strip.background = element_rect(color = col_lines, fill = "grey40", size = 1),
        plot.margin = margin(20, 20, 5, 20))

ggsave("output/2021week27_animal_rescue.pdf", width = 11, height = 8)
ggsave("output/2021week27_animal_rescue.png", width = 11, height = 8)



