# Soybean use, TidyTuesday 2021, week 06
# By Emma Skarstein, August 2022

library(tidyverse)
library(stringr)
library(showtext)

soybean_use <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-04-06/soybean_use.csv')
forest <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-04-06/forest.csv')
