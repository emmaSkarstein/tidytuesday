# Pumpkins, TidyTuesday 2021, week 43
# By Emma Skarstein, October 2021

library(tidyverse)
library(showtext)
devtools::install_github("emmaSkarstein/myGraphicsToolkit")
library(myGraphicsToolkit)

pumpkins <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-19/pumpkins.csv')

names(pumpkins)
head(pumpkins)

# Split "id" into year and type
pumpkins <- pumpkins %>% separate(id, c("year", "type"), "-")

# Fix type to something more descriptive
pumpkins$type <- recode_factor(pumpkins$type, "F" = "Field Pumpkin", "P" = "Giant Pumpkin", "S" = "Giant Squash", "W" = "Giant Watermelon", "L" = "Long Gourd", "T" = "Tomato")

# Numerical year and weight
pumpkins <- pumpkins %>% mutate(year = as.numeric(year), weight_lbs = as.numeric(weight_lbs))

# Remove watermelon, gourd and tomato and field pumpkin
#pumpkins <- pumpkins %>% filter(type %in% c("Field Pumpkin", "Giant Pumpkin", "Giant Squash"))
pumpkins <- pumpkins %>% filter(type %in% c("Giant Pumpkin", "Giant Squash"))


# Font
f1 <- "IM Fell DW Pica SC"
f2 <- "Vollkorn"
f2 <- "IM Fell DW Pica"
font_add_google(name = f1, family = f1)
font_add_google(name = f2, family = f2)
showtext_auto()

# Color palette
pumpkin_python <- '["E48532","B15D31","253534","DADBE0","6E7F86"]'
pumpkin_palette <- myGraphicsToolkit::CreateRPalette(pumpkin_python)
ShowColors(pumpkin_palette)
pumpkin_pal7 <- '["B6B2B3","D8D7DC","401105","29484A","B24317","77888F","D0732D"]'
pumpk7 <- myGraphicsToolkit::CreateRPalette(pumpkin_pal7, n = 7)
ShowColors(pumpk7)

# Colors
col_pumpk <- pumpk7[c(5,4)]
col_bg <- pumpk7[2]
col_panel <- pumpk7[1]
col_lines <- "gray5"

# Plotting
ggplot(pumpkins, aes(x = type, y = weight_lbs)) +
  geom_violin(aes(fill = type), color = "black") +
  #scale_color_manual(values = col_pumpk) +
  scale_fill_manual(values = col_pumpk) +
  ylab("weight (lbs)") +
  labs(title = "Great Pumpkins",
       subtitle = "Distributions of the weights of pumpkins and squashes, as measured \nat Great Pumpkin Commonwealth weighoffs.",
       caption = "Source: BigPumpkins.com  |  Visualization: Emma Skarstein") +
  facet_wrap(~year) +
  my_basic_theme(base_family = f2) +
  theme(text = element_text(family = f2, size = 18),
        plot.title = element_text(family = f1,
                                  size = 68,
                                  face = "bold",
                                  margin = margin(b = 5)),
        plot.subtitle = element_text(size = 17,
                                     face = "italic",
                                     margin = margin(b = 10)),
        plot.caption = element_text(size = 12),
        strip.text = element_text(size = 30, face = "bold"),
        axis.text.x = element_blank(),
        axis.title.y = element_text(face = "italic"),
        axis.text.y = element_text(size = 17),
        axis.ticks = element_blank(),
        plot.background = element_rect(fill = col_bg, color = col_bg),
        panel.background = element_rect(fill = col_panel, color = col_panel),
        legend.background = element_rect(fill = col_bg, color = col_bg),
        legend.text = element_text(size = 20, face = "bold"),
        legend.justification = "center",
        panel.spacing = unit(.05, "lines"),
        panel.border = element_rect(color = col_lines, fill = NA, size = 1),
        strip.background = element_rect(color = col_lines, size = 1),
        aspect.ratio = 1,
        plot.margin = margin(20, 25, 20, 0))

ggsave("output/2021week43_pumpkins.pdf", width = 9, height = 11)
ggsave("output/2021week43_pumpkins.png", width = 9, height = 11)

# Todo
# - facet background
# -


