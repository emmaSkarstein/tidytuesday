# Lemurs, TidyTuesday 2021, week 35
# By Emma Skarstein, October 2021

library(tidyr)
library(dplyr)
library(ggnetwork) # for plotting family graph
library(network) # for creating family graph
library(ggrepel) # to get good label placement in graph


# Get the Data
lemurs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-08-24/lemur_data.csv')


# dlc_id    dam_id    sire_id
# lemurC    lemurA    lemurB
lemurs_family <- lemurs %>% select(dlc_id, sire_id, dam_id, sex) %>% distinct()  %>%
  filter(!grepl('MULT', sire_id)) %>%
  filter(!str_detect(sire_id, "WILD|Wild|Unk|UNK")) %>%
  filter(!str_detect(dam_id, "WILD|Wild|Unk|UNK"))

lemurs_family$sex <- recode_factor(lemurs_family$sex, "F" = "Female", "M" = "Male", "ND" = "Unknown") %>%
  as.character()


# dlc_id    parent_id
# lemurC    lemurA
# lemurC    lemurB

fam_long <- lemurs_family %>%
  pivot_longer(cols = c("sire_id", "dam_id"),
                                           names_to = "parent",
                                           values_to = "parent_id")

#   A   B   C
# A 0   1   1
# B 0   0   0
# C 0   0   0

tab <- table(fam_long$dlc_id, fam_long$parent_id)
dim(tab)
length(unique(lemurs$dlc_id)) # Looks promising.

# Testing that I'm creating the family matrix correctly:
test_mat <- data.frame(dlc_id = c("01", "01", "02", "02"), parent_id = c("03", "04", "03", "05"))
test_tab <- table(test_mat$dlc_id, test_mat$parent_id)
test_tab # Looks good!


lemur_net <- network(tab, directed = TRUE)
lemur_net %v% "sex" <- fam_long$sex

lemur_font <- "Balsamiq Sans"
font_add_google(name = lemur_font, family = lemur_font)
showtext_auto()

# Plot the graph
ggplot(lemur_net, aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_edges(color = "gray6",
             arrow = arrow(length = unit(4, "pt")),  # arrows to show direction
             curvature = 0.1) +  # slight curvature (purely aesthetic)
  geom_nodes(aes(color = sex), size = 1, alpha = 0.8) +
  scale_color_manual(values = c("#D1CE45", "#4548D1", "grey")) +
  labs(title = "FAMILY NETWORK OF LEMURS",
       subtitle = "at the Duke Lemur Center",
       caption = "Source: Duke Lemur Center  |  Visualization: Emma Skarstein") +
  coord_fixed() +
  guides(colour = guide_legend(override.aes = list(size=3))) +
  theme_blank() +
  theme(text = element_text(family = lemur_font),
        plot.title = element_text(size = 35,
                                  margin = margin(b = 5),
                                  hjust = 0.5),
        plot.subtitle = element_text(size = 24,
                                     margin = margin(b = 10),
                                     hjust = 0.5),
        plot.background = element_rect(fill = "gray98", color = "gray98"),
        panel.background = element_rect(fill = "gray98"),
        legend.background = element_rect(fill = "gray98"),
        legend.text = element_text(size = 20),
        legend.title = element_blank(),
        legend.position = "top")


ggsave("output/2021week35_lemurs.pdf", width = 9, height = 11)
ggsave("output/2021week35.png")

# Todo:
# Find the individual with the most children and highlight?
# Color points by sex

