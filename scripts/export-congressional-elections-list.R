#!/usr/bin/env Rscript --vanilla

# This script takes the list of elections for each map from this repository,
# munges it into its final format and exports it to a separate directory. All
# the data processing happens in this repository so that the public-facing data
# repository can contain only data:
# <https://github.com/mapping-elections/elections-data/>.
#
# Some of this was originally part of `scripts/export-data-to-repository.R`.

library(tidyverse)
library(stringr)

states <- c("NY", "VA", "NC", "MA", "ME", "NH", "VT", "CT", "RI", "PA", "NJ",
            "DE", "MD", "SC", "GA", "AL", "IL", "IN", "KY", "LA", "MS", "MO",
            "OH", "TN")

get_county_returns_by_state <- function(state) {
  stopifnot(is.character(state) && str_count(state) == 2)
  path <- str_c("data/congressional-individual/", state)
  files <- list.files(path, pattern = "*counties*", full.names = TRUE)

  spec_counties <- cols(
    candidate_num = col_integer(),
    county = col_character(),
    election_id = col_character(),
    election_date = col_character(),
    election_year = col_integer(),
    election_type = col_character(),
    election_label = col_character(),
    office_name = col_character(),
    office_id = col_character(),
    candidate = col_character(),
    name_id = col_character(),
    affiliation = col_character(),
    affiliation_id = col_character(),
    vote = col_integer()
  )

  message("Reading county returns for ", state, " congressional elections")
  map_df(files, read_csv, col_types = spec_counties) %>%
    mutate(state = str_to_upper(str_sub(election_id, 1, 2))) %>%
    arrange(election_year, election_id)
}

congressional_counties_raw <- states %>%
  map(get_county_returns_by_state)

district_tables <- Sys.glob("data/join_tables/district/*_intermediate.csv")
election_info <- map_df(district_tables, read_csv)

elections <- congressional_counties_raw %>%
  bind_rows() %>%
  distinct(election_id, election_label, election_year, state, election_type,
           office_name)

extract_state <- function(x) {
  str_sub(x, 1, 2) %>% str_to_upper()
}

output <- elections %>%
  full_join(election_info, by = c("election_id" = "id")) %>%
  select(-state.y, -year) %>%
  rename(state = state.x) %>%
  select(election_id, election_office = office_name,
         state, congress, district, year = election_year, election_type,
         everything()) %>%
  mutate(district = if_else(district == 0L, NA_integer_, district)) %>%
  mutate(election_office = if_else(is.na(election_office), "U.S. House of Representatives", election_office)) %>%
  mutate(state = if_else(is.na(state), extract_state(election_id), state)) %>%
  arrange(state, congress, district, year)

write_csv(output, "export/elections.csv")


county_maps <- output %>%
  distinct(state, congress) %>%
  filter(!is.na(congress)) %>%
  mutate(meae_id = str_c("meae.congressional.congress",
                         str_pad(congress, 2, pad = "0"),
                         ".",
                         str_to_lower(state),
                         ".county")) %>%
  left_join(output, by = c("state", "congress")) %>%
  select(meae_id, election_id) %>%
  arrange(meae_id, election_id)

write_csv(county_maps, "export/maps-to-elections.csv")

map_list <- county_maps %>%
  distinct(meae_id) %>%
  mutate(type = "congressional",
         congressnum = str_extract(meae_id, "\\d+") %>% as.integer(),
         state = str_extract(meae_id, "\\.\\w\\w\\.") %>%
           str_replace_all("\\.", "") %>%
           str_to_upper(),
         geography = "county",
         level = "state") %>%
  arrange(state, congressnum)

write_csv(map_list, "export/maps.csv")
