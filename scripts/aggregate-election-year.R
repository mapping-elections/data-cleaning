#!/usr/bin/env Rscript
#
# Given a year's worth of CSV files for Congressional elections, aggregate them
# and make sure they are correct.

suppressPackageStartupMessages(library(readr))
suppressPackageStartupMessages(library(stringr))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(purrr))
suppressPackageStartupMessages(library(docopt))

"Aggregate county returns for a congressional elections in a year and state
and check for problems.

Usage: aggregate-election-year.R --input <input> --year <year> --state <state> --output <output>

Options:
  -i --input <input>      Path to input directory.
  -y --year <year>        Year of elections.
  -s --state <state>      Abbreviation of state of elections.
  -o --output <output>    Path to output directory.
  -h --help               Show this message.
" -> doc

opt <- docopt(doc)

stopifnot(dir.exists(opt$input))
dir.create(opt$output, showWarnings = FALSE)
opt$state <- str_to_lower(opt$state)

district_csvs <- list.files(opt$input, pattern =
                              str_c(opt$state, ".+", opt$year, "-districts\\.csv"),
                            full.names = TRUE)
counties_csvs <- list.files(opt$input, pattern =
                              str_c(opt$state, ".+", opt$year, "-counties\\.csv"),
                            full.names = TRUE)

spec_district <- cols(
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
  candidate_num = col_integer(),
  district = col_character(),
  vote = col_integer()
)

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

district <- district_csvs %>% map(read_csv, col_types = spec_district) %>% bind_rows()
counties <- counties_csvs %>% map(read_csv, col_types = spec_counties) %>% bind_rows()

district_for_cf <- district %>%
  select(election_id, candidate, name_id, affiliation, affiliation_id,
         district_vote = vote)

counties_aggregate <- counties %>%
  group_by(election_id, election_year,
           candidate, name_id, affiliation, affiliation_id) %>%
  summarize(county_vote = sum(vote, na.rm = TRUE))

stopifnot(nrow(counties_aggregate) ==
            nrow(distinct(counties_aggregate, candidate, name_id)))

replace_na <- function(x) {ifelse(is.na(x), 0L, x)}

comparison <- counties_aggregate %>%
  left_join(district_for_cf, by = c("election_id", "candidate", "name_id",
                                    "affiliation", "affiliation_id")) %>%
  mutate(difference_vote = replace_na(district_vote) - replace_na(county_vote)) %>%
  arrange(desc(abs(difference_vote))) %>%
  mutate(difference_percentage =
           round(abs(difference_vote) / pmax(replace_na(county_vote),
                                             replace_na(district_vote)),
                 2))

stopifnot(nrow(comparison) == nrow(district))

write_csv(comparison, str_c(opt$output, "/", opt$state, "-", opt$year,
                            "-congress-county2districtcf.csv"))
