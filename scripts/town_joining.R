library(readr)
library(magrittr)
library(dplyr)
library(stringr)
library(tidyr)
library(fuzzyjoin)
library(ggmap)
library(leaflet)

# Declare global variables
STATE <- "New York"
ABBR_STATE <- "NY"

#Read and clean .tsv file, cities .csv
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

names(ccd) <- names(ccd) %>%
  str_to_lower() %>%
  str_replace_all("\\.", "") %>%
  str_replace_all("\\s", "_")

# Filter to congressional election towns
cong_elect_town <- nnv %>%
  filter(office == "U.S. House of Representatives" & state == STATE &!is.na(town)) %>%
  count(town, state)

#Filter cities dataset down to the state of New York
ccd_state <- ccd %>%
  filter(st == ABBR_STATE)

# Fuzzy Join: `stringdis_left_join` of distance 1 and summary stats
ce_fjoin_1 <- cong_elect_town %>% stringdist_left_join(ccd_state, by = c("town" = "city"),
                                                  max_dist = 1, ignore_case=TRUE)

#Geocoding the fuzzy join with a distance of 1
# Total unmatched towns from `stringdist_left_join` of distance 1
unmatched_towns <- ce_fjoin_1 %>%
  select(town, id, state) %>%
  filter(is.na(id)) %>%
  count(town, state)

# Geocoding the unmatched towns and binding the two datatables together
unmatched_towns <- unmatched_towns %>%
  mutate(city_state = paste(town, state, sep = ', '))

lat_long <-  geocode(as.character(unmatched_towns$city_state), output = "more")
na_town_latlong <- bind_cols(unmatched_towns, lat_long)

# Quick and dirty map to visually check the results
leaflet(na_town_latlong) %>%
  addTiles() %>%
  addMarkers(popup = ~city_state)
