library(tidyverse)

meae_congress_counties_parties <-
  read_csv("congressional-counties-parties.csv",
           col_types = cols(
             meae_id = col_character(),
             county_ahcb = col_character(),
             county_fips = col_character(),
             districts = col_character(),
             federalist_vote = col_integer(),
             federalist_percentage = col_double(),
             antifederalist_vote = col_integer(),
             antifederalist_percentage = col_double(),
             demrep_vote = col_integer(),
             demrep_percentage = col_double(),
             chesapeake_vote = col_integer(),
             chesapeake_percentage = col_double(),
             potomac_vote = col_integer(),
             potomac_percentage = col_double(),
             repfac_vote = col_integer(),
             repfac_percentage = col_double(),
             adamsclay_vote = col_integer(),
             adamsclay_percentage = col_double(),
             jacksonian_vote = col_integer(),
             jacksonian_percentage = col_double(),
             anticaucus_vote = col_integer(),
             anticaucus_percentage = col_double(),
             caucus_vote = col_integer(),
             caucus_percentage = col_double(),
             other_vote = col_integer(),
             other_percentage = col_double(),
             county_source = col_character()
           ))

meae_congress_candidate_totals <- read_csv("congressional-candidate-totals.csv",
           col_types = cols(
             meae_id = col_character(),
             election_id = col_character(),
             candidate = col_character(),
             candidate_id = col_character(),
             district = col_character(),
             party = col_character(),
             vote = col_integer(),
             total_vote = col_integer(),
             percent_vote = col_double(),
             winner = col_logical(),
             unopposed = col_logical()
           ))

splits_parties <- split(meae_congress_counties_parties,
                        meae_congress_counties_parties$meae_id)

splits_candidates <- split(meae_congress_candidate_totals,
                           meae_congress_candidate_totals$meae_id)

walk2(names(splits_parties), splits_parties, function(id, df) {
  write_csv(df, str_glue("state-congressional-parties/{id}.csv"))
})

walk2(names(splits_candidates), splits_candidates, function(id, df) {
  write_csv(df, str_glue("state-congressional-candidates/{id}.csv"))
})

# Split counties
meae_congressional_counties <- read_csv("congressional-counties.csv",
                                        col_types = cols(
                                          election_id = col_character(),
                                          county = col_character(),
                                          state = col_character(),
                                          county_ahcb = col_character(),
                                          county_fips = col_integer(),
                                          candidate = col_character(),
                                          candidate_id = col_character(),
                                          party = col_character(),
                                          vote = col_integer()
                                        ))

meae_maps_to_elections <- read_csv("maps-to-elections.csv",
                                   col_types = cols(
                                     meae_id = col_character(),
                                     election_id = col_character())
)

meae_maps <- read_csv("maps.csv",
                      col_types = cols(
                        meae_id = col_character(),
                        type = col_character(),
                        congress = col_integer(),
                        state = col_character(),
                        geography = col_character(),
                        level = col_character())
)

counties <- meae_maps %>%
  filter(level == "state") %>%
  left_join(meae_maps_to_elections, by = "meae_id") %>%
  left_join(select(meae_congressional_counties, -state), by = "election_id") %>%
  filter(!(is.na(candidate) & vote == 0))

splits_counties <- split(counties,
                         counties$meae_id)

walk2(names(splits_counties), splits_counties, function(id, df) {
  write_csv(df, str_glue("congressional-candidate-counties-by-state/congressional-candidate-counties-{id}.csv"))
})

