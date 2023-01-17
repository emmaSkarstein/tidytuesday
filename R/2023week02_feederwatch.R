# Feederwatch, TidyTuesday 2023, week 2
# By Emma Skarstein, January 2023

library(tidyverse)
library(janitor)

feederwatch <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-01-10/PFW_2021_public.csv')
site_data <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-01-10/PFW_count_site_data_public_2021.csv')

joined <- feederwatch %>%
  janitor::clean_names() %>%
  filter(proj_period_id == "PFW_2021") %>%
  group_by(loc_id, proj_period_id) %>%
  summarise(total_birds = sum(how_many)) %>%
  ungroup() %>%
  left_join(site_data, by = c("proj_period_id", "loc_id"))

ggplot(joined, aes(x = cats, y = total_birds)) +
  geom_col()

ggplot(joined, aes(x = dogs, y = total_birds)) +
  geom_col()

ggplot(joined, aes(x = humans, y = total_birds)) +
  geom_col()
