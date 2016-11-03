library(geochecker)
library(readr)
library(magrittr)
library(dplyr)
library(tidyr)
library(stringr)
library(fuzzyjoin)
library(ggmap)

STATE <- "Massachusetts"
ABBR_STATE <- "MA"

nnv <- read_tsv("data-raw/nnv-tsv/all-votes.tsv")
ccd <- read_csv("data-raw/ccd-csv/1790-2010_MASTER.csv")


names(nnv) <- names(nnv) %>%
  str_to_lower() %>%
  str_replace_all("\\.", "") %>%
  str_replace_all("\\s", "_")

# Assign city values to town column for computational ease
nnv <- nnv %>%
  mutate(town = ifelse(is.na(town), city, town))

# Extract the year from the date column and treat that as an integer
nnv <- nnv %>%
  mutate(year = str_extract(date, "\\d{4}") %>% as.integer())

nnv <- nnv %>%
  mutate(town = ifelse(is.na(town), city, town))

names(ccd) <- names(ccd) %>%
  str_to_lower() %>%
  str_replace_all("\\.", "") %>%
  str_replace_all("\\s", "_")

maine_counties <- c("Cumberland", "Hancock", "Kennebeck", "Lincoln", "Oxford",
                    "Penobscot", "Somerset", "Washington", "York")

mass_counties <- c("Barnstable","Berkshire","Bristol","Devonshire","Dukes","Essex",
                   "Franklin","Hampden","Hampshire","Middlesex","Nantucket","Norfolk",
                   "Plymouth","Suffolk","Worcester", "Worcester North")

elect_town_me <- nnv %>%
  filter(state == STATE,
         !is.na(town),
         county %in% maine_counties) %>%
  count(town, state)

elect_town_ma <- nnv %>%
  filter(state == STATE,
         !is.na(town),
         !county %in% maine_counties) %>%
  count(town, state)

#Filter cities dataset down to the state of New York
ccd_state <- ccd %>%
  filter(st == "ME")

#Fuzzy Join: `stringdis_left_join` of distance 1 and summary stats
ce_fjoin_1 <- elect_town_me %>% stringdist_left_join(ccd_state, by = c("town" = "city"),
                                                       max_dist = 1, ignore_case=TRUE)

#Geocoding the fuzzy join with a distance of 1
#Total unmatched towns from `stringdist_left_join` of distance 1
unmatched_towns <- ce_fjoin_1 %>%
  select(town, id, state) %>%
  filter(is.na(id)) %>%
  count(town, state)

#Geocoding the unmatched towns and binding the two datatables together
unmatched_towns <- unmatched_towns %>%
  mutate(state = "Maine") %>%
  mutate(city_state = paste(town, state, sep = ', '))


lat_long <-  geocode(as.character(unmatched_towns$city_state), output = "more")
na_town_latlong <- bind_cols(unmatched_towns, lat_long)

na_town_latlong <- na_town_latlong %>%
  select(town, state, city_state, lat, lon, administrative_area_level_3, administrative_area_level_2, administrative_area_level_1)

corrected_me <- geocheck(na_town_latlong, zoom = 9, tile_provider = "Esri.WorldTopoMap")

corrected_me <- read_csv("data/town-georeferenced/ma_me_geochecker.csv")

corrected_me <- geocheck(corrected_me, zoom = 9, tile_provider = "Esri.WorldTopoMap")

write_csv(corrected_me, "data/town-georeferenced/ma_me_geochecker.csv")


corrected_ma <- read_csv("data/town-georeferenced/ma_geochecker.csv")



fjoined_towns <- ce_fjoin_1 %>%
filter(!is.na(lat)) %>%
  select(town,state,lat,lon)

ny_towns_geolocated <- corrected %>%
  select(town, state, lat, lon)

nnv_ny_towns <- nnv %>%
  filter (state=="New York", !is.na(town))

bounded_towns <- bind_rows(elect_town_me, unmatched_towns) %>%
  count(town)

joined_towns <- nnv_ny_towns %>%
  left_join(ny_towns_geolocated, by=c("town", "state"))
