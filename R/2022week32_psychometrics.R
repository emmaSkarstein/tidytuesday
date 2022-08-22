# Open Psychometrics, TidyTuesday 2022, week 32
# By Emma Skarstein, August 2022

library(tidyverse)
library(stringr)
library(showtext)

characters <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-08-16/characters.csv')
psych_stats <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-08-16/psych_stats.csv')

unique(characters$uni_name)

stranger_things <- characters %>% filter(uni_name == "Stranger Things")

stranger_things_psych <- psych_stats %>% filter(uni_name == "Stranger Things")


