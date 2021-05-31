
library(readr)
library(dplyr)
library(stringr)
library(lubridate)

# get data ----------------------------------------------------------------

artists <- read_csv(
  "https://club-cooking-datasette-3ec5cj2jvq-nw.a.run.app/club-bookings/.%2Fartists.csv?_stream=on"
  )

events <- read_csv(
  "https://club-cooking-datasette-3ec5cj2jvq-nw.a.run.app/club-bookings/.%2Fevents.csv?_stream=on"
)

# tidy --------------------------------------------------------------------

lineups <- events %>% 
  select(-rowid) %>% 
  dplyr::filter(club_id == 237) %>% 
  inner_join(
    select(artists, -rowid), by = c("club_id"="club_id", "event_id"="event_id")
  )

lineups_clean <- lineups %>% 
  dplyr::filter(
    str_detect(event_name, regex("rescheduled|postponed|cancelled", ignore_case = TRUE),
               negate = TRUE)
  ) %>% 
  mutate(day = wday(event_date, label = TRUE))

write_csv(lineups_clean, "data/lineups.csv")
