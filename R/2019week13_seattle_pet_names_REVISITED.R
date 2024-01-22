# Seattle pet names, TidyTuesday 2019, week 13
# By Emma Skarstein, November 2023
# Note: I used this data in 2021, but in this script I look more at finding a
# cute cat name from the data set.

library(tidyverse)
#library(packcircles)
#library(viridis)
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

# We have some preferred letters and letter combinations
preferred_letters <- "ka|st|mo"
names_with_preferred_letters <- name_counts %>% filter(grepl(preferred_letters, animals_name))
