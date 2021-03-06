library(geochecker)
library(readr)
library(magrittr)
library(dplyr)
library(tidyr)
library(stringr)
library(fuzzyjoin)
library(ggmap)

# Global Variables
STATE <- "New York"
ABBR_STATE <- "NY"

NNV <- read_tsv("data-raw/nnv-tsv/all-votes.tsv")
CCD <- read_csv("data-raw/ccd-csv/1790-2010_MASTER.csv")
DUPLICATE <- read_csv("data/town-georeferenced/ny_duplicate.csv")

# Clean NNV and CCD (standardize variable case, populate town variable)
names(NNV) <- names(NNV) %>%
  str_to_lower() %>%
  str_replace_all("\\.", "") %>%
  str_replace_all("\\s", "_")

NNV <- NNV %>%
  mutate(town = ifelse(is.na(town), city, town)) %>%
  mutate(year = str_extract(date, "\\d{4}") %>% as.integer())

names(CCD) <- names(CCD) %>%
  str_to_lower() %>%
  str_replace_all("\\.", "") %>%
  str_replace_all("\\s", "_")

# function to change the write out .csv file name according to your global variables
create_intermediate_filename <- function(state_abbr) {
path_to_output <- "data/town-georeferenced/"
file_suffix <- "_intermediate-table.csv"
str_c(path_to_output, str_to_lower(state_abbr), file_suffix)
}

create_coordinates_filename <- function(state_abbr) {
  path_to_output <- "data/town-georeferenced/"
  file_suffix <- "_coordinates.csv"
  str_c(path_to_output, str_to_lower(state_abbr), file_suffix)
}

create_id <- function(x, state) {
  stringr::str_c("ME", "-", state,
                 stringr::str_pad(x, width = 4, side = "left", pad = "0"))
}

# Filtering tables down to specific state
nnv_state <- NNV %>%
  filter(state == STATE,
         !is.na(town))

ccd_state <- CCD %>%
  filter(st == ABBR_STATE)

# Fixing duplicates (joining duplicate table to nnv_state and
# then filtering down the correct town names)
duplicate_join <- nnv_state %>%
  left_join(DUPLICATE, by = c("town"="nnv_town"))

clean_town <- duplicate_join %>%
  mutate(standardized_town = ifelse(is.na(standardized_town), town, standardized_town))

joining_towns <- unique(clean_town[,c('standardized_town', 'state')])

clean_town_b <- unique(clean_town[,c('standardized_town','town','county')])

# Fuzzy Join of distance 1 and filtering to unmatched towns
fuzzy_join <- joining_towns %>% stringdist_left_join(ccd_state, by = c("standardized_town" = "city"),
                                                     max_dist = 1, ignore_case=TRUE) %>%
  filter(!is.na(id))

straight_join <- joining_towns %>%
  left_join(ccd_state, by =c("standardized_town" = "city"))

unmatched_towns <- straight_join %>%
  select(standardized_town, id, state) %>%
  filter(is.na(id)) %>%
  mutate(state = STATE,
         city_state = paste(standardized_town, state, sep = ', '))

# Geocode with Google API
lat_long <-  geocode(as.character(unmatched_towns$city_state), output = "more")

geocoded_town <- bind_cols(unmatched_towns, lat_long) %>%
  select(standardized_town, state, city_state, lat, lon, administrative_area_level_2,
         administrative_area_level_1)

georeferenced <- geocheck(geocoded_town, zoom = 9, tile_provider = "Esri.WorldTopoMap")
#write_csv(georeferenced, "data/town-georeferenced/ct_geochecker.csv")

# Write out intermediate table
intermediate_table <- duplicate_join %>%
  mutate(standardized_town = ifelse(is.na(standardized_town), town, standardized_town)) %>%
  count(town, standardized_town, state) %>%
  select(town, standardized_town, state)
write_csv(intermediate_table, create_intermediate_filename(ABBR_STATE))

# Joining the georeferenced table back to NNV
fuzzyjoined_towns <- fuzzy_join %>%
  filter(!is.na(id))

towns_geolocated <- georeferenced %>%
  ungroup() %>%
  select(standardized_town, state, lat, lon)

total_towns <- bind_rows(fuzzyjoined_towns, towns_geolocated) %>%
  mutate(lat = ifelse(is.na(lat), lat_bing, lat),
         lon = ifelse(is.na(lon), lon_bing, lon)) %>%
  select (standardized_town, state, lat, lon)

write_csv(total_towns, create_coordinates_filename(ABBR_STATE))




# FOR PREVIOUSLY GEOREFERNCED STATES ONLY
# Correcting duplicates and joining coordinates

# Read in Geochecker .csv and georefernce towns
georeferenced <- read_csv("data/town-georeferenced/ny_coordinates.csv")

standardized_towns <- unmatched_towns %>%
  left_join(georeferenced, by = c("standardized_town" = "town", "state" = "state")) %>%
  select(standardized_town, state, lat, lon, checked) %>%
  arrange(standardized_town)

sub_standardized_a <- standardized_towns %>%
  filter(is.na(lat)) %>%
  mutate(town_state = paste(standardized_town, state, sep = ', ')) %>%
  select(standardized_town,state, town_state)

sub_latlon <- geocode(as.character(sub_standardized$town_state), output = "more")

sub_standardized <- bind_cols(sub_standardized,sub_latlon) %>%
  select(standardized_town, state, town_state, lon, lat, administrative_area_level_2,
         administrative_area_level_1)

sub_standardized <- geocheck(sub_standardized, zoom = 9, tile_provider = "Esri.WorldTopoMap")

located_towns <- standardized_towns %>%
  filter(!is.na(lat))

standardized_towns_b <- bind_rows(located_towns, sub_standardized) %>%
  select(standardized_town, state, lat, lon, checked)

standardized_towns_c <- geocheck(standardized_towns_b, zoom = 9, tile_provider = "Esri.WorldTopoMap")

only_straight_join<- straight_join %>%
  filter(!is.na(id))

final_product <- bind_rows(standardized_towns_c, only_straight_join) %>%
  select(standardized_town, state, id, lat, lon, checked)

#write_csv(final_product,create_intermediate_filename(ABBR_STATE))

final_product <- final_product %>%
  arrange(state, standardized_town) %>%
  mutate(me_town_id = create_id(1:nrow(.), "NY"))

join_back <- unique(clean_town[,c('standardized_town','town','county',"state")])

join_back <- join_back %>%
  left_join(final_product, by = c("standardized_town" = "standardized_town"))

write_csv(join_back, create_intermediate_filename("NY_ID"))

standardized_towns <- duplicate_join %>%
  mutate(standardized_town = ifelse(is.na(standardized_town), town, standardized_town))

fuzzyjoined_towns <- fuzzy_join %>%
  filter(!is.na(id)) %>%
  ungroup() %>%
  mutate (lat = lat_bing,
          lon = lon_bing)

total_towns <- bind_rows(fuzzyjoined_towns, standardized_towns)

unique_id <- read_csv("data/town-georeferenced/ny_id_intermediate-table.csv")

unique_sub_a <-  clean_town %>%
  left_join(unique_id, by=c("standardized_town", "town", "county"))

unique_sub_b <- unique_sub_a %>%
  filter(!is.na(me_town_id)) %>%
  mutate(date = as.integer(date))

home_stretch <- read_csv("data/town-georeferenced/ny_lastly.csv")

TA_DA <- bind_rows(unique_sub_b, home_stretch) %>%
  mutate(ccd_id = id.y,
         id = id.x,
         state = state.x) %>%
  select(id, town, county, state, lat, lon, ccd_id, me_town_id)

TA_DA_a <- unique(TA_DA[,c("id", "town", "county", "state","ccd_id", "lat", "lon","me_town_id")])



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
fuzzy_join <- elect_town_me %>% stringdist_left_join(ccd_state, by = c("town" = "city"),
                                                       max_dist = 1, ignore_case=TRUE)

#Geocoding the fuzzy join with a distance of 1
#Total unmatched towns from `stringdist_left_join` of distance 1
unmatched_towns <- fuzzy_join %>%
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
