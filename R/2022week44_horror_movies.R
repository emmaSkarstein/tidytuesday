# Horror movies, TidyTuesday 2022, week 44
# By Emma Skarstein, November 2022

library(tidyverse)
library(showtext)
library(ggtext)

showtext_auto()
showtext_opts(dpi = 300)

horror_movies <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-11-01/horror_movies.csv')

movies <- horror_movies %>%
  mutate(new_date = substring(as.character(release_date), 6) %>% as.factor()) %>%
  group_by(new_date) %>%
  summarise(n = n()) %>%
  filter(new_date != "01-01") %>%
  separate(new_date, c('month', 'day'), remove = FALSE) %>%
  mutate(is_october = (month == "10"),
         is_first = (day == "01"))

month_nums <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")
month_breakpoints <- c(paste0(month_nums, "-", "01"), "12-31")
month_breaks <- data.frame(start = month_breakpoints[1:12],
                           end = month_breakpoints[2:13],
                           mid = paste0(month_nums, "-", "03"),
                           month = c("January", "February", "March", "April",
                                     "May", "June", "July", "August",
                                     "September", "October", "November",
                                     "December")) %>%
  mutate(month = factor(month, levels = c("January", "February", "March", "April",
                                          "May", "June", "July", "August",
                                          "September", "October", "November",
                                          "December")))
# Colors
col_text <- "grey70"

# Font
f1 <- "Lacquer" # Title font
font_add_google(name = f1, family = f1)
f2 <- "Schoolbell" # Body text font
font_add_google(name = f2, family = f2)

ggplot() +
  # Points for release numbers
  geom_point(data = movies, aes(x = new_date, y = n, fill = is_october),
             shape = 21, color = "black", size = 3) +
  # Points for 1st day of the month highlights
  geom_point(data = filter(movies, new_date == "03-01"),
             aes(x = new_date, y = n),
             color = "grey70", fill = NA,
             shape = 21, size = 5) +
  # Point for halloween highlight
  geom_point(data = filter(movies, new_date == "10-31"),
             aes(x = new_date, y = n),
             color = "#C70039", fill = NA,
             shape = 21, size = 5) +
  # Month lables
  geom_text(data = month_breaks,
            aes(x = mid, y = -20, label = month),
            color = col_text, family = f1, size = 3.5,
            hjust = 0, angle = 0) +
  # Annotation: 1st day per month
  annotate(geom = "text", x = "03-01", y = 280,
           family = f2, color = col_text, hjust = 0, size = 3.5,
           label = str_wrap("There is a spike on the 1st of each month, maybe this is a more popular release day, or it may be recorded like that if the true date is missing.",
                            width = 50)) +
  # Annotation: Halloween
  annotate(geom = "text", x = "10-31", y = 490,
           family = f2, color = col_text, hjust = 1, size = 3.5,
           label = str_wrap("Halloween is by FAR the most popular horror movie release day!",
                            width = 40)) +
  # Blood drops
  annotate(geom = "text", x = c("01-24", "02-03"), y = c(680, 665),
             label = c(";", "'"), size = 10, family = f1, color = "#C70039") +
  # Scales
  scale_x_discrete(breaks = month_breakpoints, drop = FALSE) +
  scale_y_continuous(breaks = seq(0, 500, by = 100)) +
  scale_fill_manual(values = c("grey80", "#C70039")) +
  # Lables
  ylab("Number of\n movies released") +
  labs(title = "Sp<i style='color:#C70039'>oo</i>ky movie season!",
       subtitle = str_wrap("These are the release days of around 35K horror movies from The Movie Database (ignoring the year). <br>Not surprisingly, there is a definite spike in horror movie releases in <i style='color:#C70039'>October</i>, with the most popular <br>release-day being Halloween.",
                           width = 100),
       caption = "Source: The Movie Database, via Tanya Shapiro | Graphics: Emma Skarstein") +
  #
  coord_cartesian(ylim = c(0, 530), clip = "off") +
  theme_minimal() +
  theme(text = element_text(color = col_text, family = f2, size = 15),
        plot.title = element_markdown(family = f1, size = 40),
        plot.subtitle = element_markdown(family = f2, size = 15),
        plot.caption = element_text(size = 10, margin = margin(t = 15)),
        legend.position = "none",
        axis.text = element_text(color = col_text),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_text(angle = 0, hjust = 1),
        plot.background = element_rect(fill = "black"),
        panel.grid = element_blank(),
        panel.grid.major.x = element_line(color = "grey20"),
        plot.margin = margin(20, 50, 10, 20))


ggsave("output/2022week44_horror_movies.png", width = 12, height = 7)
ggsave("output/2022week44_horror_movies.pdf", width = 12, height = 7)

