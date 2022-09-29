# Pride Donations, TidyTuesday 2022, week 23
# By Emma Skarstein, August 2022


library(tidyverse)  # Data cleaning tools and ggplot2
library(stringr)
library(showtext)   # Fonts
library(janitor)    # Cleaning names
library(ggtext)     # element_markdown
library(patchwork)

pride_aggregates <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-06-07/pride_aggregates.csv')
fortune_aggregates <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-06-07/fortune_aggregates.csv')
static_list <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-06-07/static_list.csv')
pride_sponsors <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-06-07/pride_sponsors.csv')
corp_by_politician <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-06-07/corp_by_politician.csv')
donors <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-06-07/donors.csv')

# Loading fonts
f1 <- "EB Garamond"
f2 <- "Playfair Display"

font_add_google(name = f1, family = f1)
font_add_google(name = f2, family = f2)
showtext_auto()

# Colors
col_bar <- "#191919"
col_bgr <- "#F0F0F0"
col_text <- "#191919"
col_brands <- c(toyota = "#FE0300", at_t = "#0CA8E0", comcast = "",
                   amazon = "#FF9904", fedex = "#662C8F", state_farm = "#EC1E25",
                   general_motors = "#1272CE", budweiser = "#D00E23" )

# Companies that sponsored pride, but also donated to anti-LGBTQ campaigns

companies_contributions <- static_list %>% janitor::clean_names()
names(companies_contributions)

sizes <- 50*c(1,1,1,1,0.8,1,1,1,1,0.8,1,1,0.5,1,0.5)

companies_contributions <- companies_contributions %>%
  filter(pride) %>%
  filter(amount_contributed_across_states > 10000) %>%
  mutate(image = paste0("R/2022week23_logos/", make_clean_names(company), ".png")) %>%
  mutate(image_html = paste0("<img src='", image,  "' width='", sizes, "' /><br>")) %>%
  mutate(brand_color = c("#FE0300", "#0CA8E0", "#0FB24A", "#FF9904", "#662C8F", "#EC1E25", "#1272CE", "#D00E23", "#139858", "#7B847C", "#D01E0F", "#1273CE", "#1577C0", "#614F45", "#FBB713"))

p <- ggplot(companies_contributions,
       aes(x = reorder(company, amount_contributed_across_states),
           y = amount_contributed_across_states)) +
  geom_col(aes(fill = I(brand_color)), color = col_bar, width = 0.75) +
  geom_text(aes(label = scales::dollar(amount_contributed_across_states),
                hjust = c(1.1, 1.1, rep(-0.1, 13)),
                color = I(c("grey98", "grey98", rep(col_text, 13)))),
            size = 5, family = f1) +
  scale_y_continuous(limits = c(0, 650000),
                     expand = c(0.01, 10)) +
  scale_x_discrete(name = NULL, labels = rev(companies_contributions$image_html)) +
  coord_flip() +
  annotate("text", x = 10, y = 610000, label = "Proud or not?",
           family = f2, fontface = "bold", size = 12, hjust = 1) +
  annotate("text", x = 8, y = 610000, label = str_wrap("Despite publicly supporting Pride, these companies have all donated over $10,000 in support to anti-LGBTQ+ politicians in the USA.",
                                                       width = 40),
           family = f1, size = 6, hjust = 1) +
  annotate("text", x = 1, y = 610000, label = "Source: Data For Progress  |  Visualization: Emma Skarstein",
           family = f1, size = 3.5, hjust = 1)+
  theme_bw() +
  theme(text = element_text(family = f1, color = col_text, size = 15),
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

p
ggsave("output/2022week23_pride_donations.pdf", width = 8, height = 9)

png("output/2022week23_pride_donations.png", width = 8, height = 9,
    res = 400, units = "in")
print(p)
dev.off()






