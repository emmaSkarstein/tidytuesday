# Boardgames, TidyTuesday 2022, week 4
# By Emma Skarstein, August 2022

library(tidyverse)
library(stringr)
library(showtext)
library(gt)
library(gtExtras)

ratings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-25/ratings.csv')
details <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-25/details.csv')

unique(ratings$name)
summary(ratings$year)
unique(ratings$year)

# Cooperative games, high ratings, old games, etc.
# Expansions vs popularity

old_games <- ratings %>% filter(year < 1960 & year> 1900 ) %>% left_join(details, by = "id")

our_games_names <- c("Carcassonne", "Pandemic", "Ticket to Ride: Europe", "Ghost Stories",
               "Blueprints", "Lost Cities", "Railroad Ink: Deep Blue Edition", "My City",
               "Welcome To...", "Dominion", "One Deck Dungeon", "KeyForge: Age of Ascension",
               "Food Chain Magnate")
our_games <- ratings %>% filter(name %in% our_games_names)
# Merge with details set


# Cooperative games
coop_det <- details %>% filter(str_detect(boardgamemechanic, "Cooperative"))

cooperative_games <- left_join(coop_det, ratings, by = "id")

top_coop <- cooperative_games %>% filter(rank<42)

# Re-formatting the boardgamecategory column
cat_vec <- top_coop$boardgamecategory %>%
  substr(2, nchar(top_coop$boardgamecategory) - 1) %>%
  strsplit(",") %>%
  sapply(paste, collapse=" ") %>%
  str_replace_all("'", "") %>%
  str_replace_all("  ", ", ")

table_data <- top_coop %>%
  # Transform variables
  mutate(
    # Make range of min-max number of players.
    player_range = paste(minplayers, "-", maxplayers),
    # Add formatted category
    categories = cat_vec) %>%
  # Select relevant columns
  select(name, thumbnail, categories, yearpublished, player_range,
                    playingtime, bayes_average)



table_data %>%
  gt() %>%
  # Thumbnail images from url
  gt_img_rows(thumbnail, height = 50) %>%
  # Place year under name of game
  gt_merge_stack(col1 = name, col2 = yearpublished,
                 font_size = c("20px", "14px")) %>%
  # Color the rating column
  data_color(columns = bayes_average,
             colors = c("#FFE2C5", "#C10A0A"),
             alpha = 0.7) %>%
  # Center the column with thumbnail, as well as numeric columns
  cols_align("center", columns = c("thumbnail", "player_range", "playingtime", "bayes_average")) %>%
  cols_width(c(categories) ~ px(250),
             c(player_range, playingtime, bayes_average) ~ px(90)) %>%
  # Rename columns
  cols_label(name = "",
             thumbnail = "",
             categories = "Categories",
             player_range = "Number of players",
             playingtime = "Time (minutes)",
             bayes_average = "Rating") %>%
  # Add title and subtitle
  tab_header(title = "Great Cooperative Games",
             subtitle = "The top ten highest rated cooperative board games") %>%
  # Add source
  tab_source_note(source_note = "Source: BoardGamesGeeks (via [TidyTuesday](https://github.com/rfordatascience/tidytuesday/blob/master/data/2022/2022-01-25/readme.md)") %>%
  # Style options
  opt_all_caps() %>%
  opt_table_font(font = list(google_font("Chivo"),
                             default_fonts()))

gtsave()
