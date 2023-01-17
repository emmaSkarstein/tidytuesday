# Weather forecasts, TidyTuesday 2022, week 51
# By Emma Skarstein, January 2023

library(tidyverse)


weather_forecasts <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-12-20/weather_forecasts.csv')
cities <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-12-20/cities.csv')
outlook_meanings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-12-20/outlook_meanings.csv')


forecast_12 <- weather_forecasts %>%
  filter(forecast_hours_before == 12)

select_cities <- cities %>% filter(city %in% city_list)

weather_cities <- weather_forecasts %>% left_join(cities, by = c("city", "state"))

ggplot(weather_cities,
       aes(x = lon, y = lat)) +
  geom_point()

# Looking at the forecast vs observed in Madison

alaska <- weather_forecasts %>%
  filter(city == "ANCHORAGE", forecast_hours_before == 12) %>%
  pivot_longer(cols = c("observed_temp", "forecast_temp"),
               values_to = "temperature", names_to = "forecast_or_observation") %>%
  mutate(temperature_type = paste0(high_or_low, forecast_or_observation))

ggplot(alaska,
       aes(x = date, y = temperature, group = temperature_type)) +
  geom_line(aes(color = temperature_type))

alaska_errors <- weather_forecasts %>%
  filter(city == "ANCHORAGE", forecast_hours_before == 12) %>%
  mutate(error = abs(forecast_temp - observed_temp))

ggplot(alaska_errors, aes(x = date, y = error, color = high_or_low)) +
  geom_line() +
  geom_smooth()

