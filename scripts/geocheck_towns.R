library(geochecker)
library(readr)
library(magrittr)
library(dplyr)
library(tidyr)
library(stringr)
library(fuzzyjoin)
library(ggmap)

STATE <- "New York"
ABBR_STATE <- "NY"

nnv <- read_tsv("data-raw/nnv-tsv/all-votes.tsv")
ccd <- read_csv("data-raw/ccd-csv/1790-2010_MASTER.csv")
duplicate <- read_csv("data/town-georeferenced/ny_duplicate.csv")

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

nnv_town <- nnv %>%
  filter(state == STATE,
         !is.na(town))

join_duplicate <- nnv_town %>%
  left_join(duplicate, by = c("town"="nnv_town"))

clean_town <- join_duplicate %>%
  mutate(corrected_town = ifelse(is.na(corrected_town), town, corrected_town)) %>%
  count(corrected_town, state)

ccd_state <- ccd %>%
  filter(st == ABBR_STATE)

ce_fjoin_1 <- clean_town %>% stringdist_left_join(ccd_state, by = c("corrected_town" = "city"),
                                                     max_dist = 1, ignore_case=TRUE)
# Geocoding the fuzzy join with a distance of 1
# Total unmatched towns from `stringdist_left_join` of distance 1
unmatched_towns <- ce_fjoin_1 %>%
  select(corrected_town, id, state) %>%
  filter(is.na(id)) %>%
  count(corrected_town, state)

# Geocoding the unmatched towns and binding the two datatables together
unmatched_towns <- unmatched_towns %>%
  mutate(state = STATE) %>%
  mutate(city_state = paste(corrected_town, state, sep = ', '))

lat_long <-  geocode(as.character(unmatched_towns$city_state), output = "more")
na_town_latlong <- bind_cols(unmatched_towns, lat_long)

na_town_latlong <- na_town_latlong %>%
  select(corrected_town, state, city_state, lat, lon, administrative_area_level_2, administrative_area_level_1)

georeferenced <- geocheck(na_town_latlong, zoom = 9, tile_provider = "Esri.WorldTopoMap")
#write_csv(georeferenced, "data/town-georeferenced/ct_geochecker.csv")


# Read in Geochecker .csv and georefernce towns
georeferenced <- read_csv("data/town-georeferenced/ny_geochecker.csv")
georeferenced <- geocheck(corrected, zoom = 9, tile_provider = "Esri.WorldTopoMap")
#write_csv(georeferenced, "data/town-georeferenced/ct_geochecker.csv")


# Joining the georeferenced table back to NNV
fjoined_towns <- ce_fjoin_1 %>%
  filter(!is.na(id))

towns_geolocated <- georeferenced %>%
  ungroup() %>%
  select(corrected_town, state, lat, lon)

total_towns <- bind_rows(fjoined_towns, towns_geolocated) %>%
  mutate(lat = ifelse(is.na(lat), lat_bing, lat),
         lon = ifelse(is.na(lon), lon_bing, lon))

#write_csv(bounded_towns, "data/town-georeferenced/va_geochecker.csv")


# FOR PREVIOUSLY GEOREFERNCED STATES ONLY
# Correcting duplicates and joining coordinates
corrected_towns <- unmatched_towns %>%
  left_join(corrected, by = c("corrected_town" = "town", "state" = "state")) %>%
  select(corrected_town, state, lat, lon)

corrected_towns <- join_duplicate %>%
  mutate(corrected_town = ifelse(is.na(corrected_town), town, corrected_town))

fuzzyjoined_towns <- ce_fjoin_1 %>%
  filter(!is.na(id)) %>%
  ungroup() %>%
  mutate (lat = lat_bing,
          lon = lon_bing)

total_towns <- bind_rows(fuzzyjoined_towns, geocheck_join)



#FOR MASSACHUSETTS AND MAINE
# Separating the maine and mass counties
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
  filter(st == ABBR_STATE)

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
  mutate(state = STATE) %>%
  mutate(city_state = paste(town, state, sep = ', '))


lat_long <-  geocode(as.character(unmatched_towns$city_state), output = "more")
na_town_latlong <- bind_cols(unmatched_towns, lat_long)

na_town_latlong <- na_town_latlong %>%
  select(town, state, city_state, lat, lon, administrative_area_level_3,
         administrative_area_level_2, administrative_area_level_1)

corrected_me <- geocheck(na_town_latlong, zoom = 9, tile_provider = "Esri.WorldTopoMap")

corrected_me <- read_csv("data/town-georeferenced/ma_me_geochecker.csv")
corrected_me <- geocheck(corrected_me, zoom = 9, tile_provider = "Esri.WorldTopoMap")

corrected_ma <- read_csv("data/town-georeferenced/ma_ma_geochecker.csv")
corrected_ma <- geocheck(corrected_ma, zoom = 9, tile_provider = "Esri.WorldTopoMap")

# write_csv(corrected_ma, "data/town-georeferenced/ma_ma_geochecker.csv")
