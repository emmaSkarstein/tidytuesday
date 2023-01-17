# Bigfoot, TidyTuesday 2022, week 37
# By Emma Skarstein, January 2023

library(janeaustenr)
library(tidytext)
library(tidyverse)

bigfoot <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-09-13/bigfoot.csv')
bigfoot <- bigfoot %>% filter(classification != "Class C")

book_words <- austen_books() %>%
  unnest_tokens(word, text) %>%
  count(book, word, sort = TRUE)

total_words <- book_words %>%
  group_by(book) %>%
  summarize(total = sum(n))

book_words <- left_join(book_words, total_words)


bigfoot_words <- bigfoot %>%
  unnest_tokens(word, observed) %>%
  count(classification, word, sort = TRUE)

total_words <- bigfoot_words %>%
  group_by(classification) %>%
  summarize(total = sum(n))

bigfoot_words <- left_join(bigfoot_words, total_words)

bigfoot_tf_idf <- bigfoot_words %>%
  bind_tf_idf(word, classification, n)

bigfoot_tf_idf %>%
  select(-total) %>%
  arrange(desc(tf_idf))



library(tidytext)
library(tidylo)

bigfoot %>%
  unnest_tokens(word, observed) %>%
  count(classification, word) %>%
  filter(n > 100) %>%
  bind_log_odds(classification, word, n) %>%
  arrange(-log_odds_weighted)

library(ggthemes)

bigfoot %>%
  filter(longitude > -130) %>%
  ggplot(aes( x = longitude, y = latitude, color = classification)) +
  borders("state") +
  geom_point(alpha = 0.7, size = 1, stroke = 0.1) +
  coord_map() +
  theme_bw()

ggsave("output/2022week37_bigfoot.png")
