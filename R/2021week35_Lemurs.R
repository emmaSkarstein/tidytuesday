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
test_mat <- data.frame(dlc_id = c("A", "A", "D", "D"), parent_id = c("B", "C", "B", "E"))
test_tab <- table(test_mat$dlc_id, test_mat$parent_id)
test_tab # Looks good!

test_mat <- data.frame(dlc_id = c("01", "01", "02", "02"), parent_id = c("03", "04", "03", "05"))
test_tab <- table(test_mat$dlc_id, test_mat$parent_id)
test_tab # Looks good!


lemur_net <- network(tab, directed = TRUE)

font_add_google(name = "Nunito", family = "Nunito")
showtext_auto()

# Plot the graph
ggplot(lemur_net, aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_edges(color = "gray3",
             arrow = arrow(length = unit(4, "pt")),  # arrows to show direction
             curvature = 0.1) +  # slight curvature (purely aesthetic)
  geom_nodes(color = "#D1CE45", size = 1, alpha = 0.8) +
  labs(title = "FAMILY NETWORK OF LEMURS",
       subtitle = "At the Duke Lemur Center",
       caption = "Source: Duke Lemur Center  |  Visualization: Emma Skarstein") +
  theme_blank() +
  theme(text = element_text(family = "Nunito"))

# Todo:
# Find the individual with the most children and highlight?
#

ggsave("output/2021week35_lemurs.pdf")
ggsave("output/2021week35.png")



