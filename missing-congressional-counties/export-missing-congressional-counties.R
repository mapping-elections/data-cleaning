library(tidyverse)
library(stringr)
source("R/helpers.R")

candidates <- read_csv("../elections-data/candidates.csv")

read_county_join_codes <- function(path) {
  state <- path %>% basename() %>% str_sub(1, 2) %>% str_to_upper()
  df <- read_csv(path, col_types = cols_only(nnv_county = col_character(),
                                             ahcb_county = col_character())) %>%
    mutate(state = state) %>%
    select(county = nnv_county, county_ahcb = ahcb_county, state)
}

ahcb <- USAboundariesData::counties_historical_lores %>%
  select(name, state_terr, id, fips) %>%
  mutate(state_terr = as.character(state_terr))

county_codes <- list.files("data/join_tables/county", full.names = TRUE) %>%
  map_df(read_county_join_codes) %>%
  left_join(USAboundaries::state_codes, by = c("state" = "state_abbr")) %>%
  left_join(ahcb, by = c("county_ahcb" = "name", "state_name" = "state_terr")) %>%
  select(-state_name, -jurisdiction_type, -state_code)

dir.create("missing-congressional-counties/data", recursive = TRUE, showWarnings = FALSE)
missing_cc <- read_csv("missing-congressional-counties/missing-congressional-counties.csv")

files_to_export <- str_c(missing_cc$election_id, ".xml")

run_export <- function(f) {
  cmd <- str_c("./scripts/xml2table.R data-raw/nnv-xml/", f,
               " -o missing-congressional-counties/data/")
  message(cmd)
  system(cmd)
}

walk(files_to_export, run_export)

csvs <- Sys.glob("missing-congressional-counties/data/*-counties.csv")

raw_data <- map_df(csvs, read_csv, col_types =
                     cols(
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
)

losers <- raw_data %>%
  group_by(election_id) %>%
  summarize(keep = any(!is.na(vote))) %>%
  filter(!keep)

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
           candidate = if_else(is.na(candidate),
                                    backup_name, candidate))
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
           candidate = candidate, candidate_id = name_id,
           party = affiliation, party_id = affiliation_id,
           vote) %>%
    mutate(candidate = strip_null(candidate),
           candidate_id = strip_null(candidate_id),
           party = strip_null(party),
           party_id = strip_null(party_id))
}



df <- raw_data %>%
  anti_join(losers, by = "election_id") %>%
  mutate(state = str_to_upper(str_sub(election_id, 1, 2))) %>%
  arrange(election_year, election_id) %>%
  cleanup_names() %>%
  join_to_geocodes() %>%
  normalize_county_returns()

# Then append that data frame to the congressional-counties.csv file

