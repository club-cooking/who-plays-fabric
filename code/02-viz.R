
library(readr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(hrbrthemes)

# get data ----------------------------------------------------------------

lineups <- read_csv("data/lineups.csv")

# analysis ----------------------------------------------------------------

artist_debuts <- lineups %>% 
  group_by(artist_id) %>% 
  summarise(debut_date = min(event_date), year = year(debut_date)) %>% 
  count(year, name = "n_debuts")

n_events <- lineups %>% 
  mutate(year = year(event_date)) %>% 
  group_by(year) %>% 
  summarise(n_events = n_distinct(event_id))

artist_debuts %>% 
  left_join(n_events, by = "year") %>% 
  mutate(debuts_per_event = n_debuts / n_events) %>% 
  dplyr::filter(year != min(year)) %>% 
  ggplot(aes(x = year, y = debuts_per_event)) +
  geom_line() +
  geom_smooth() +
  labs(title = "the # of artists making their debut at fabric has been in decline since the '05 high point",
       subtitle = "Almost two (1.9) new artists were playing a fabric event for the first time across 2005. In 2020, this figure had fallen to 1.3.",
       y = "# fabric debuts per event") +
  theme_ipsum() +
  theme(plot.title.position = "plot")
