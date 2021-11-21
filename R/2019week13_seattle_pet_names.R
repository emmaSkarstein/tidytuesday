# Seattle pet names, TidyTuesday 2019, week 13
# By Emma Skarstein, November 2021

library(tidyverse)
library(packcircles)
library(viridis)
library(showtext)

seattle_pets <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-03-26/seattle_pets.csv")

seattle_pets <- seattle_pets %>%
  mutate(year = as.numeric(str_sub(license_issue_date, -4)))

cats <- seattle_pets %>% filter(species == "Cat") %>%
  mutate(name_length = as.factor(str_length(animals_name)))

name_counts <- cats %>%
  drop_na(animals_name) %>%
  count(animals_name, sort = TRUE) %>%
  mutate(animals_name = fct_reorder(animals_name, desc(-n)))

unique(cats$primary_breed)

# For each name_length, assign each cat a value 1:number of cats with that name length.
fancycats <- cats %>%
  drop_na(animals_name) %>%
  filter(!grepl("Domestic|Mix", primary_breed))

name_length_cats <- fancycats %>%
  group_by(name_length) %>%
  mutate(index = row_number()) %>%
  right_join(fancycats)

ggplot(name_length_cats) +
  geom_point(aes(x = name_length, y = index, color = primary_breed), size = 2, alpha = 0.5)

ggplot(name_length_cats) +
  geom_bar(aes(x = name_length, fill = primary_breed))

breed_counts <- cats %>% count(primary_breed) %>%
  mutate(primary_breed = fct_reorder(primary_breed, desc(-n)))

ggplot(breed_counts %>% filter(!grepl("Domestic|Mix", primary_breed))) +
  geom_col(aes(x = n, y = primary_breed))

ggplot(name_counts[1:10,]) +
  geom_col(aes(x = n, y = animals_name)) +
  theme(axis.title = element_blank())

# Done exploring, this is where it starts. Plotting circles proportional to name popularity.

n = 100
# Generate the layout. This function return a dataframe with one line per bubble.
# It gives its center (x and y) and its radius, proportional of the value
packing <- circleProgressiveLayout(name_counts$n[1:n], sizetype='area')

# We can add these packing information to the initial data frame
data <- cbind(name_counts[1:n,], packing)

# Check that radius is proportional to value. We don't want a linear relationship,
# since it is the AREA that must be proportionnal to the value
plot(data$radius, data$n)

# The next step is to go from one center + a radius to the coordinates of a circle that
# is drawn by a multitude of straight lines.
dat.gg <- circleLayoutVertices(packing, npoints=50)

# Font
f1 <- "Josefin Sans"
f2 <- "Open Sans"
font_add_google(name = f1, family = f1)
font_add_google(name = f2, family = f2)
showtext_auto()

# Colors
col_bg <- "grey97"

# Make the plot
ggplot() +
  # Make the bubbles
  geom_polygon(data = dat.gg, aes(x, y, group = id, fill=as.factor(id)), colour = "black", alpha = 0.6) +
  scale_fill_viridis(option = "rocket", discrete = TRUE, direction = -1) +
  # Add text in the center of each bubble + control its size
  geom_text(data = data, aes(x, y, size=n*4, label = animals_name, family = f1, fontface = "bold")) +
  scale_size_continuous(range = c(2, 6)) +
  labs(title = "Seattle Kitties Luna, Lily and Lucy",
       subtitle = "Top 100 names for cats registered in Seattle from 2015 to 2018.",
       caption = "Source: seattle.gov  |  Visualization: Emma Skarstein") +
  # Luna annotation
  annotate(geom = "text", x = 39, y = 40, label = "Luna is the most popular, \nwith 111 cats carrying the name.",
           family = f2, hjust = 1, size = 4.5) +
  # Bear annotation
  annotate(geom = "curve", x = -7, y = -36, xend = -6, yend = -34,
           curvature = .25, arrow = arrow(length = unit(2, "mm"))) +
  annotate(geom = "text", x = -16, y = -36, label = "24 Bears have snuck in!", family = f2) +
  # Cookie annotation
  annotate(geom = "curve", x = -34, y = 24, xend = -32, yend = 21,
           curvature = .25, arrow = arrow(length = unit(2, "mm"))) +
  annotate(geom = "text", x = -34, y = 25.5, label = "21 Cookies...", family = f2) +
  # Oreo annotation
  annotate(geom = "curve", x = -32, y = -26, xend = -28, yend = -24,
           curvature = -.25, arrow = arrow(length = unit(2, "mm"))) +
  annotate(geom = "text", x = -32, y = -27.5, label = "...and 21 Oreos.", family = f2) +
  # Also annotation
  geom_text(aes(x = 39, y = -33, label = "Also... \n13 cats are named Cat. \n16 cats are named Mouse. \n16 cats are named Monkey. ",
                hjust = 1, lineheight = 1, family = f2), size = 4) +
  # General theme:
  theme_void() +
  theme(text = element_text(family = "Yuji", size = 18),
        plot.title = element_text(family = f1,
                                      size = 35,
                                      face = "bold",
                                      margin = margin(b = 5)),
        plot.subtitle = element_text(size = 18,
                                         margin = margin(b = 10)),
        plot.caption = element_text(size = 12,
                                    margin = margin(t = 60)),
        plot.background = element_rect(fill = col_bg, color = col_bg),
        panel.background = element_rect(fill = col_bg, color = col_bg),
        legend.position="none",
        plot.margin = margin(40, 20, 20, 20)) +
  coord_equal()


ggsave("output/2019week13_seattle_pet_names.pdf", width = 9, height = 11)
ggsave("output/2019week13_seattle_pet_names.png", width = 9, height = 11)



# Looking at the most unique names
unique_names <- name_counts %>% filter(n == 1)
names2 <- name_counts %>% filter(n == 2)


