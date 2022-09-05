# Pride Donations, TidyTuesday 2022, week 23
# By Emma Skarstein, August 2022

remotes::install_github("hrbrmstr/ggchicklet")

library(tidyverse)  # Data cleaning tools and ggplot2
#library(stringr)
library(showtext)   # Fonts
library(janitor)    # Cleaning names
#library(ggimage)
library(ggtext)     # element_markdown
library(ggchicklet) # Rounded corners on bar charts

pride_aggregates <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-06-07/pride_aggregates.csv')
fortune_aggregates <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-06-07/fortune_aggregates.csv')
static_list <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-06-07/static_list.csv')
pride_sponsors <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-06-07/pride_sponsors.csv')
corp_by_politician <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-06-07/corp_by_politician.csv')
donors <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-06-07/donors.csv')

# Loading fonts
f1 <- "Open Sans"
f2 <- "Spectral SC"

font_add_google(name = f1, family = f1)
font_add_google(name = f2, family = f2)
showtext_auto()

# Colors
col_bar <- "grey80"
col_bgr <- "white"
col_text <- "grey10"

# Companies that sponsored pride, but also donated to anti-LGBTQ campaigns

companies_contributions <- static_list %>% janitor::clean_names()
names(companies_contributions)

sizes <- 70*c(1,1,1,1,0.8,1,1,1,1,0.8,1,1,0.5,1,0.5)

companies_contributions <- companies_contributions %>%
  filter(pride) %>%
  filter(amount_contributed_across_states > 10000) %>%
  mutate(image = paste0("R/2022week23_logos/", make_clean_names(company), ".png")) %>%
  mutate(image_html = paste0("<img src='", image,  "' width='", sizes, "' /><br>"))

ggplot(companies_contributions,
       aes(x = reorder(company, amount_contributed_across_states),
           y = amount_contributed_across_states)) +
  geom_chicklet(width = 0.75, color = col_bar, fill = col_bar) +
  geom_text(aes(label = scales::dollar(amount_contributed_across_states)),
            hjust = -0.1, size = 5, color = col_text) +
  scale_y_continuous(limits = c(0, 700000),
                     expand = c(0.01, 10)) +
  scale_x_discrete(name = NULL, labels = rev(companies_contributions$image_html)) +
  labs(title = "Companies continue to donate to anti-LGBTQ \ncampaigns while supporting Pride",
       subtitle = "These companies have donated over $10,000 to anti-LGBTQ campaigns.") +
  coord_flip() +
  theme_bw() +
  theme(text = element_text(family = f1, color = col_text, size = 15),
        plot.title = element_text(family = f1, color = col_text, size = 18),
        plot.subtitle = element_text(family = f1, color = col_text, size = 12),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_markdown(vjust = 0.7),
        axis.text.x = element_blank(),
        axis.ticks = element_blank(),
        plot.margin = unit(c(1,1,1,1), "cm"),
        legend.position = "none",
        panel.background = element_rect(color = col_bgr, fill = col_bgr),
        plot.background = element_rect(color = col_bgr, fill = col_bgr),
        panel.grid = element_blank(),
        panel.border = element_blank())

ggsave("output/2022week23_pride_donations.pdf", width = 8, height = 11)
ggsave("output/2022week23_pride_donations.png", width = 8, height = 11)












