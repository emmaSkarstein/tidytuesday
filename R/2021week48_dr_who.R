# Doctor Who, TidyTuesday 2021, week 48
# By Emma Skarstein, November 2021

library(tidyverse)
library(showtext)
library(paletteer)
library(ggpubr) # For brackets



# Read in the data manually
directors <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-11-23/directors.csv')
episodes <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-11-23/episodes.csv')
writers <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-11-23/writers.csv')
imdb <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-11-23/imdb.csv')


# 1. Rating for each writer
writer_ratings <- full_join(episodes, writers, by = "story_number")

top_writers <- writers %>% count(writer) %>% filter(n >= 4)

top_writer_ratings <- writer_ratings %>% filter(writer %in% top_writers$writer)

averages <- top_writer_ratings %>% group_by(writer) %>% summarize(mean = mean(rating, na.rm = TRUE))


ggplot(top_writer_ratings, aes(x = rating, y = writer)) +
  geom_jitter(width = 0.2, height = 0.3, alpha = 0.6) +
  geom_point(data = averages, aes(x = mean, y = writer), color = "red", size = 3)



# 2. Timeseries, rating for each episode
str(episodes)
episodes$season_number <- as.factor(episodes$season_number)

season_breaks <- episodes %>% group_by(season_number) %>% summarise(from = min(first_aired), to = max(first_aired))
season_breaks <- data.frame(
  from = as.Date(c('2004-07-01', '2005-10-01', '2006-10-01', '2007-10-01', '2010-02-01', '2010-10-01', '2011-11-01', '2014-07-01', '2014-12-01', '2016-08-01', '2018-03-01', '2019-07-01', '2021-05-01')),
  to = as.Date(c('2005-10-01', '2006-10-01', '2007-10-01', '2008-10-01', '2010-10-01', '2011-11-01', '2013-08-01', '2014-12-01', '2016-08-01', '2018-03-01', '2019-07-01', '2021-05-01', '2022-03-01')),
  lab = as.factor(1:13))

# Colors
pal <- paletteer_d("dutchmasters::milkmaid")
col_bg <- "grey95"
col_line <- "grey90"

# Font
f1 <- "Oswald"
f2 <- "Titillium Web"
font_add_google(name = f1, family = f1)
font_add_google(name = f2, family = f2)
showtext_auto()

ggplot() +
  geom_rect(data = season_breaks, aes(xmin = from, xmax = to, ymin = -Inf, ymax = Inf, fill = lab), alpha = 0.15) +
  geom_point(data = episodes, aes(x = first_aired, y = rating, size = uk_viewers), color = "white") +
  geom_point(data = episodes, aes(x = first_aired, y = rating, fill = season_number, size = uk_viewers), shape = 21, color = "black", alpha = 0.85) +
  geom_bracket(
    xmin = as.Date('2004-09-01'), xmax = as.Date('2005-08-01'), y.position = 74,
    label = "Ninth doctor", tip.length = -0.03, vjust = 3, family = f1, color = "grey50") +
  geom_bracket(
    xmin = as.Date('2005-12-01'), xmax = as.Date('2009-12-01'), y.position = 74,
    label = "Tenth doctor", tip.length = -0.03, vjust = 3, family = f1, color = "grey50") +
  geom_bracket(
    xmin = as.Date('2010-04-01'), xmax = as.Date('2014-05-01'), y.position = 74,
    label = "Eleventh doctor", tip.length = -0.03, vjust = 3, family = f1, color = "grey50") +
  geom_bracket(
    xmin = as.Date('2014-09-01'), xmax = as.Date('2018-01-01'), y.position = 74,
    label = "Twelfth doctor", tip.length = -0.03, vjust = 3, family = f1, color = "grey50") +
  geom_bracket(
    xmin = as.Date('2018-04-01'), xmax = as.Date('2022-01-01'), y.position = 74,
    label = "Thirteenth doctor", tip.length = -0.03, vjust = 3, family = f1, color = "grey50") +
  # Annotation Love & Monsters
  annotate(geom = "text", x = as.Date('2006-09-01'), y = 76, label = "Episode 'Love & Monsters', a surprising low \nin a otherwise highly rated season!",
           family = f2, size = 3, hjust = 0) +
  geom_point(aes(x = as.Date("2006-06-17"), y = 76), shape = 21, size = 6) +
  # Annotation top episodes
  annotate(geom = "text", x = as.Date('2008-10-01'), y = 91, label = "The all-time highest rated episodes are 'The Stolen Earth' \nand 'Journey\'s end', which conclude season 4.",
           family = f2, size = 3, hjust = 0) +
  geom_point(aes(x = as.Date("2008-07-05"), y = 91), shape = 21, size = 8) +
  # Annotation special episodes
  annotate(geom = "text", x = as.Date('2014-03-23'), y = 88, label = "The gray points are special episodes. \nThese generally have particularly high viewing numbers.",
           family = f2, size = 3, hjust = 0) +
  geom_point(aes(x = as.Date("2013-11-23"), y = 88), shape = 21, size = 8) +
  # Annotation first season
  annotate(geom = "text", x = as.Date('2005-08-01'), y = 80, label = "The ratings for episodes in the first revival season are almost increasing for each episode. \nMaybe people just needed to get back into the Doctor Who mood?",
           family = f2, size = 3, hjust = 0) +
  geom_point(aes(x = as.Date("2005-04-09"), y = 80), shape = 21, size = 7) +
  # Scales
  scale_fill_manual(values = pal, na.value = "grey70") +
  scale_y_continuous(limits = c(73, 92), breaks = seq(70, 92, by = 2)) +
  scale_x_date(limits = as.Date(c('2004-07-01', '2022-03-01')), breaks = "1 year", date_labels = "%Y", expand = c(0,0)) +
  xlab("Date first aired") +
  ylab("Rating") +
  labs(title = "Doctor Who ratings are decreasing",
       subtitle = "The Doctor Who revival seasons (2005 - today) have been well received, but the last few seasons are seeing less of the \nsucces that met the first seasons. Point size indicates UK viewing numbers.",
       caption = "Source: datardis package by Jonathan Kitt  |  Visualization: Emma Skarstein") +
  theme(text = element_text(family = f2, size = 12),
        plot.title = element_text(family = f1,
                                      size = 40,
                                      face = "bold",
                                      margin = margin(b = 5)),
        plot.subtitle = element_text(size = 15,
                                         margin = margin(b = 10)),
        plot.caption = element_text(size = 8,
                                    margin = margin(t = 10)),
        panel.grid = element_line(color = col_line),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.y = element_text(angle = 0, family = f1),
        axis.title.x = element_text(hjust = 1, family = f1),
        plot.background = element_rect(fill = col_bg, color = col_bg),
        panel.background = element_rect(fill = col_bg, color = col_line),
        legend.position = "none",
        plot.margin = margin(20, 20, 5, 20))

ggsave("output/2021week48_dr_who.pdf", width = 12, height = 7)
ggsave("output/2021week48_dr_who.png", width = 12, height = 7)
