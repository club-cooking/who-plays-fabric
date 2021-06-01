
library(readr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(hrbrthemes)

# get data ----------------------------------------------------------------

lineups <- read_csv("data/lineups.csv")

residents <- c("craigrichards", "terryfrancis", "jacobhusley",
               "peterpixzel", "cormac")

# analysis ----------------------------------------------------------------

# artists at least 50 apps. 
lineups %>% 
  group_by(artist_id) %>% 
  summarise(
    apps = n_distinct(event_id), debut = first(event_date), artist_name = first(artist_name)
    ) %>% 
  arrange(desc(apps)) %>% 
  dplyr::filter(apps >= 50) %>% 
  mutate(resident = (artist_id %in% residents)) %>% 
  ggplot(aes(x = reorder(artist_name, apps), y = apps, fill = resident)) +
  geom_col() +
  coord_flip()

# # artist debuts x year
artist_debuts_year <- lineups %>% 
  group_by(artist_id) %>% 
  summarise(debut_date = min(event_date), year = year(debut_date)) %>% 
  count(year, name = "n_debuts")

n_events_year <- lineups %>% 
  mutate(year = year(event_date)) %>% 
  group_by(year) %>% 
  summarise(n_events = n_distinct(event_id))

artist_debuts_year %>% 
  left_join(n_events_year, by = "year") %>% 
  mutate(debuts_per_event = n_debuts / n_events) %>% 
  dplyr::filter(year != min(year)) %>% 
  ggplot(aes(x = year, y = debuts_per_event)) +
  geom_line() +
  geom_smooth() +
  scale_y_continuous(limits = c(0, NA)) +
  labs(title = "the # of artists making their debut at fabric has been in decline since the '05 high point",
       subtitle = "Almost two (1.9) new artists were playing a fabric event for the first time across 2005. In 2020, this figure had fallen to 1.3.",
       y = "# fabric debuts per event") +
  theme_ipsum() +
  theme(plot.title.position = "plot")

# # artists x month of year
artist_debuts_month <- lineups %>% 
  group_by(artist_id) %>% 
  summarise(debut_date = min(event_date), month = month(debut_date, label = TRUE)) %>% 
  count(month, name = "n_debuts")

n_events_month <- lineups %>% 
  mutate(month = month(event_date, label = TRUE)) %>% 
  group_by(month) %>% 
  summarise(n_events = n_distinct(event_id))

artist_debuts_month %>% 
  left_join(n_events_month, by = "month") %>% 
  mutate(debuts_per_event = n_debuts / n_events) %>% 
  ggplot(aes(x = month, y = debuts_per_event)) +
  geom_col() +
  coord_polar()

# # artists by day of week
artist_debuts_day <- lineups %>% 
  group_by(artist_id) %>% 
  summarise(debut_date = min(event_date), day = wday(debut_date, label = TRUE)) %>% 
  count(day, name = "n_debuts")

n_events_day <- lineups %>% 
  mutate(day = wday(event_date, label = TRUE)) %>% 
  group_by(day) %>% 
  summarise(n_events = n_distinct(event_id))

artist_debuts_day %>% 
  left_join(n_events_day, by = "day") %>% 
  mutate(debuts_per_event = n_debuts / n_events) %>%
  dplyr::filter(day %in% c("Fri", "Sat", "Sun")) %>% 
  ggplot(aes(x = reorder(day, -debuts_per_event), y = debuts_per_event)) +
  geom_col()
