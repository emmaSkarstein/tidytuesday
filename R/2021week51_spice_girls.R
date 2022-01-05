# Spice girls, TidyTuesday 2021, week 51
# By Emma Skarstein, November 2021

library(tidyverse)


studio_album_tracks <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-12-14/studio_album_tracks.csv')
lyrics <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-12-14/lyrics.csv')
related_artists <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-12-14/related_artists.csv')


spice_df <- studio_album_tracks %>% select(album_release_year, track_name, album_name,
                                           track_number, danceability, energy,
                                           key, loudness, mode, speechiness,
                                           acousticness, instrumentalness,
                                           liveness, tempo, valence, duration_ms) %>%
  mutate(x_pos = album_release_year + track_number/12 - 0.45)


ggplot(spice_df, aes(y = x_pos)) +
  geom_point(aes(x = duration_ms/100000)) +
  theme(axis.title = element_blank())
