# Ravelry yarn, TidyTuesday 2022, week 41
# By Emma Skarstein, October 2022

# Sentiment analysis:
# https://www.tidytextmining.com/sentiment.html


library(tidyverse)
library(stringr)
library(tidytext)
library(packcircles)
library(showtext)
library(ggpattern)
library(colorspace)
library(paletteer)

library(janeaustenr)

yarn <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-10-11/yarn.csv')


tidy_yarn <- yarn |>
  unnest_tokens(word, name)

nrc_joy <- get_sentiments("nrc") %>%
  filter(sentiment == "joy")

joyful_yarn <- tidy_yarn |>
  inner_join(nrc_joy) |>
  count(word, sort = TRUE)

get_sentiments("nrc") |> select(sentiment) |> unique()


# Pleasant ply

n = 50
# Generate the layout. This function return a dataframe with one line per bubble.
# It gives its center (x and y) and its radius, proportional of the value
packing <- circleProgressiveLayout(joyful_yarn$n[1:n], sizetype='area')

# We can add these packing information to the initial data frame
data <- cbind(joyful_yarn[1:n,], packing)

# Check that radius is proportional to value. We don't want a linear relationship,
# since it is the AREA that must be proportional to the value
plot(data$radius, data$n)

# The next step is to go from one center + a radius to the coordinates of a circle that
# is drawn by a multitude of straight lines.
dat.gg <- circleLayoutVertices(packing, npoints=50) |>
  mutate(id_factor = as.factor(id))

# Font
f1 <- "Averia Serif Libre"
f1 <- "Single day"
f1 <- "Potta One"
f1 <- "Sacramento"
f1 <- "Signika Negative" # Bubble text font
f3 <- "Miniver" # Title font
f2 <- "Alegreya"
f2 <- "Nunito" # Body text font
font_add_google(name = f1, family = f1)
font_add_google(name = f2, family = f2)
font_add_google(name = f3, family = f3)
showtext_auto()
showtext_opts(dpi = 300)

# Colors
pal <- rep(paletteer_d("tvthemes::Day"), 13)[1:n+1]
pal_dark <- darken(pal, amount = 0.1)
col_text <- paletteer_d("tvthemes::Day")[1]

# Make the plot
p <- ggplot() +
  # Make the bubbles
  geom_polygon_pattern(data = dat.gg,
                       aes(x, y,
                           pattern_fill = id_factor,
                           fill = id_factor,
                           pattern_spacing = 1/id,
                           pattern_angle = sample(c(15, 20, 30, 60),
                                                  size = nrow(dat.gg),
                                                  replace = TRUE)),
               pattern_size = 0.07,
               pattern = "crosshatch", pattern_density = 0.6) +
  scale_pattern_spacing_continuous(range = c(0.005, 0.012)) +
  scale_fill_manual(values = pal_dark) +
  scale_pattern_fill_manual(values = pal) +
  # Add text in the center of each bubble + control its size
  geom_text(data = data,
            aes(x, y, size=n*4, label = word, family = f1)) +
  scale_size_continuous(range = c(3, 10)) +
  labs(title = "Thrilling Threads",
       subtitle = "Top 100 joyful words in yarn names from ravelry.com.",
       caption = "Source: ravelry.com  |  Graphic: Emma Skarstein") +
  # General theme:
  theme_void() +
  theme(text = element_text(family = f2, size = 18),
        plot.title = element_text(family = f3,
                                  size = 50,
                                  color = col_text),
        plot.subtitle = element_text(size = 18,
                                     color = col_text),
        plot.caption = element_text(size = 12,
                                    color = col_text,
                                    margin = margin(t = 60)),
        legend.position="none",
        plot.margin = margin(40, 20, 20, 20)) +
  coord_equal()

ggimage::ggbackground(p, background = "R/2022week41_yarn/wood.jpg")

ggsave("output/2022week41_yarn.png", width = 9, height = 11)
ggsave("output/2022week41_yarn.pdf", width = 9, height = 11)

