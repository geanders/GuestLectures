# Pull data on flood-related storm events for Colorado for 2013 to 2015

# If needed, install `noaastormevents`
# devtools::install_github("zailchen/noaastormevents")
library(noaastormevents)
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)

co_floods <- create_storm_data(date_range = c("2013-01-01", "2015-12-31")) %>% 
  setNames(tolower(names(.))) %>% 
  filter(state == "COLORADO" & event_type %in% c("Flood", "Flash Flood")) %>% 
  mutate(begin_day = str_pad(begin_day, 2, pad = "0")) %>%
  mutate(end_day = str_pad(end_day, 2, pad = "0")) %>%
  mutate(cz_fips = str_pad(cz_fips, 3, pad = "0")) %>%
  unite(begin_date, begin_yearmonth, begin_day, sep = "") %>% 
  unite(end_date, end_yearmonth, end_day, sep = "") %>% 
  unite(fips, state_fips, cz_fips, sep = "") %>% 
  select(begin_date, end_date, event_type, fips, cz_name, 
         deaths_direct, injuries_direct, damage_property, damage_crops, 
         source, begin_lat, begin_lon, end_lat, end_lon, flood_cause, 
         episode_narrative, event_narrative) %>% 
  mutate(begin_date = ymd(begin_date), 
         end_date = ymd(end_date),
         cz_name = str_to_title(cz_name),
         fips = as.numeric(fips))

co_tornadoes <- create_storm_data(date_range = c("2013-01-01", "2015-12-31")) %>% 
  setNames(tolower(names(.))) %>% 
  filter(state == "COLORADO" & event_type %in% c("Tornado", "Funnel Cloud")) %>% 
  mutate(begin_day = str_pad(begin_day, 2, pad = "0")) %>%
  mutate(end_day = str_pad(end_day, 2, pad = "0")) %>%
  mutate(cz_fips = str_pad(cz_fips, 3, pad = "0")) %>%
  unite(begin_date, begin_yearmonth, begin_day, sep = "") %>% 
  unite(end_date, end_yearmonth, end_day, sep = "") %>% 
  unite(fips, state_fips, cz_fips, sep = "") %>% 
  select(begin_date, end_date, event_type, fips, cz_name, 
         deaths_direct, injuries_direct, damage_property, damage_crops, 
         source, begin_lat, begin_lon, end_lat, end_lon, flood_cause, 
         episode_narrative, event_narrative) %>% 
  mutate(begin_date = ymd(begin_date), 
         end_date = ymd(end_date),
         cz_name = str_to_title(cz_name),
         fips = as.numeric(fips))

save(co_floods, file = "data/co_floods.Rdata")
save(co_tornadoes, file = "data/co_tornadoes.Rdata")
