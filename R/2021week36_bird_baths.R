# Bird baths, TidyTuesday 2021, week 36
# By Emma Skarstein, October 2021

library(tidyverse)


bird_baths <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-08-31/bird_baths.csv')

names(bird_baths)

bird_df <- bird_baths %>%
  pivot_longer(names_to = "bird_type", values_to = "bird_count", cols = 4:last_col()) %>%
  janitor::clean_names()

bird_df
