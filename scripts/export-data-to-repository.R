#!/usr/bin/env Rscript --vanilla
# This script takes the data from this repository, munges it into its final format
# and exports it to a separate directory. All the data processing happens in this
# repository so that the public-facing data repository can contain only data:
# <https://github.com/mapping-elections/elections-data/>.

# STATES to export
states <- c("NY", "VA", "NC", "MA", "ME", "NH", "VT", "CT", "RI", "PA", "NJ",
            "DE")

suppressMessages(library(tidyverse))
suppressMessages(library(stringr))
suppressMessages(library(USAboundaries))
suppressMessages(library(humaniformat))
suppressMessages(library(xml2))
source("R/helpers.R")

# Setup the export directories
if (dir.exists("export"))
  unlink("export", recursive = TRUE)
dir.create("export/congressional/county", recursive = TRUE, showWarnings = FALSE)

# Create the candidate records from the candidate authority files
message("Parsing candidate authority records")
names_dir <- "data-raw/nnv-names/candidate"
name_authorities <- list.files(names_dir, full.names = TRUE, pattern = "\\.xml")

parse_name_auth <- function(x) {
  candidate_name <- x %>%
    xml_find_all("auth:candidate") %>%
    xml_attr("name")
  candidate_id <- x %>%
    xml_find_all("auth:candidate") %>%
    xml_attr("id")
  tibble(candidate_id, candidate_name)
}

candidates <- name_authorities %>%
  map(read_xml) %>%
  map_df(parse_name_auth) %>%
  arrange(candidate_id) %>%
  filter(candidate_id != "AA0000")

# Read in the county join codes for each state
read_county_join_codes <- function(path) {
  state <- path %>% basename() %>% str_sub(1, 2) %>% str_to_upper()
  df <- read_csv(path, col_types = cols_only(nnv_county = col_character(),
                                             ahcb_county = col_character())) %>%
    mutate(state = state) %>%
    select(county = nnv_county, county_ahcb = ahcb_county, state)
}

ahcb <- USAboundaries::hist_us_counties@data %>%
  select(name, state_terr, id, fips) %>%
  mutate(state_terr = as.character(state_terr))

county_codes <- list.files("data/join_tables/county", full.names = TRUE) %>%
  map_df(read_county_join_codes) %>%
  left_join(state_codes, by = c("state" = "state_abbr")) %>%
  left_join(ahcb, by = c("county_ahcb" = "name", "state_name" = "state_terr")) %>%
  select(-state_name, -jurisdiction_type, -state_code)

# TODO Create the party table

# TODO Create the office table

# Read in the election returns for each county by state. We have to keep the
# states separate so that we can write them as separate files.
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

# Clean up the problems with names
cleanup_names <- function(df) {
  # sample_before_names <<- df
  df %>%
    # AA0000 is for candidate whose names are parts of other, real candidates'
    # names. Scattering is for conglomerations of votes.
    mutate(name_id = if_else(name_id == "AA0000",
                             "Other candidates", name_id),
           name_id = if_else(candidate == "scattering",
                             "Other candidates", name_id)) %>%
    filter(!is.na(name_id)) %>%
    # Now the names have to be aggregated so that there aren't any duplicates
    group_by(election_id, name_id, affiliation_id, county) %>%
    summarize(vote = sum(vote, na.rm = TRUE),
              affiliation = sort(affiliation)[1],
              state = unique(state),
              election_year = unique(election_year),
              backup_name = most_common_in_vector(candidate)) %>%
    ungroup() %>%
    # Now the candidate IDs can be joined to the candidates table to get the name
    left_join(candidates, by = c("name_id" = "candidate_id")) %>%
    mutate(backup_name = reverse_name(backup_name),
           candidate_name = if_else(is.na(candidate_name),
                                    backup_name, candidate_name))
}

# Join to the geographic codes
join_to_geocodes <- function(df) {
  df %>% left_join(county_codes, by = c("county" = "county", "state" = "state"))
}

# Clean up the data to just the information that we want. Most of the
# information we are dropping belongs in other tables. We are keeping the
# election_id, to join to the election table. The county field lets us join to
# the county join table. The candidate_id table lets us join to the candidate
# table, but for convenience we will also keep the candidate name. We keep party
# ID to join to the parties table, but keep the party name for convenience. Vote
# is an integer of the votes, with NAs for missing values.
normalize_county_returns <- function(df) {
  df %>%
    arrange(election_year, election_id, county) %>%
    select(election_id,
           county, state, county_ahcb = id, county_fips = fips,
           candidate = candidate_name, candidate_id = name_id,
           party = affiliation, party_id = affiliation_id,
           vote) %>%
    mutate(candidate = strip_null(candidate),
           candidate_id = strip_null(candidate_id),
           party = strip_null(party),
           party_id = strip_null(party_id))
}

# Actually read in the files and run the cleaning functions on them
congressional_counties_raw <- states %>%
  map(get_county_returns_by_state)
congressional_counties <- congressional_counties_raw %>%
  map(cleanup_names) %>%
  map(join_to_geocodes) %>%
  map(normalize_county_returns)
names(congressional_counties) <- states

write_county_csv <- function(state, df) {
  path <- str_c("export/congressional/county/", "congressional-counties-",
                str_to_lower(state), ".csv")
  message("EXPORTING ", path)
  write_csv(df, path)
}

message("EXPORTING export/congressional-counties.csv")
walk2(names(congressional_counties), congressional_counties, write_county_csv)
congressional_counties2 <- congressional_counties %>% bind_rows()
write_csv(congressional_counties2, "export/congressional-counties.csv")

candidates2 <- congressional_counties2 %>%
  distinct(candidate_id, candidate) %>%
  arrange(candidate_id) %>%
  filter(!is.na(candidate_id))
message("EXPORTING export/candidates.csv")
write_csv(candidates2, "export/candidates.csv")

get_district <- function(x) {
  stopifnot(is.character(x))
  x <- str_split_fixed(x, ", ", 2)
  x[ , 2, drop = TRUE] %>% str_replace_all(", Ballot.+", "")
}

message("EXPORTING export/elections.csv")
congressional_counties_raw %>%
  bind_rows() %>%
  distinct(election_id, election_label, election_year, state, election_type,
           office_name) %>%
  arrange(state, election_year, election_id) %>%
  mutate(district = get_district(election_label)) %>%
  write_csv("export/elections.csv")

message("COPYING to data repository")
system("cp -r export/* ../elections-data/")
