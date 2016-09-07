# Read in all the NY congressional elections at the county level and create a
# single table
library(dplyr)
library(readr)
library(purrr)

files <- list.files("data/congressional-individual/NY", pattern = "*counties*",
                    full.names = TRUE)

spec_counties <- cols(
  candidate_num = col_integer(),
  county = col_character(),
  election_id = col_character(),
  election_date = col_integer(),
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

counties <- map_df(files, read_csv, col_types = spec_counties) %>%
  arrange(election_year, election_label)

write_csv(counties, "data/congressional/ny-congressional-counties.csv")
