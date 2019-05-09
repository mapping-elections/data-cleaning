library(tidyverse)
library(sf)

STATE <- "Maryland"
meae_id <- "meae.staterepresentative.1808.md.county"
candidates_out <- "md1808-candidate-totals.csv"
counties_out <- "md1808-counties-parties.csv"
county_in_files <- Sys.glob("data/state-legislative/md/md.*.1808-counties.csv")
candidate_in_files <- Sys.glob("data/state-legislative/md/md.*.1808-overview.csv")

join_table <- read_csv("data/join_tables/county/md_county_join_table.csv") %>%
  select(-notes) %>%
  rename(ahcb_intermediate = ahcb_county)

county_shp <- USAboundariesData::counties_historical_lores
sf::st_geometry(county_shp) <- NULL
county_shp <- county_shp %>% filter(state_terr == STATE) %>%
  select(name, ahcb_county = id, county_fips = fips) %>% distinct()

counties_raw <- county_in_files %>% map_df(read_csv, na = c("null", "NA"), col_types =
                                         cols(
                                           candidate_num = col_integer(),
                                           county = col_character(),
                                           election_id = col_character(),
                                           election_date = col_double(),
                                           election_year = col_double(),
                                           election_type = col_character(),
                                           election_label = col_character(),
                                           office_name = col_character(),
                                           office_id = col_character(),
                                           candidate = col_character(),
                                           name_id = col_character(),
                                           affiliation = col_character(),
                                           affiliation_id = col_character(),
                                           vote = col_integer()
                                         ))

overview_raw <- candidate_in_files %>% map_df(read_csv, na = c("null", "NA"), col_types =
                                                cols(
                                                  election_id = col_character(),
                                                  election_date = col_double(),
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
                                                  overview_vote = col_integer()
                                                ))

candidates_total <- overview_raw %>%
  mutate(meae_id = meae_id) %>%
  rename(candidate_id = name_id, vote = overview_vote) %>%
  group_by(meae_id, election_id, candidate, candidate_id, affiliation, affiliation_id) %>%
  summarize(vote = sum(vote, na.rm = TRUE)) %>%
  group_by(meae_id, election_id) %>%
  mutate(total_vote = sum(vote, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(percent_vote = round(vote / total_vote, 3),
         winner = if_else(percent_vote > 0.5, TRUE, FALSE),
         unopposed = if_else(percent_vote == 1.0, TRUE, FALSE)) %>%
  rename(party = affiliation) %>%
  mutate(party = if_else(party == "Democrat", "Democratic-Republican", party),
          party = if_else(party == "Republican", "Democratic-Republican", party)) %>%
  arrange(meae_id, election_id, desc(vote)) %>%
  select(-affiliation_id)

counties_tidy <- counties_raw %>%
  mutate(meae_id = meae_id) %>%
  rename(party = affiliation) %>%
  group_by(meae_id, county, party) %>%
  summarize(vote = sum(vote, na.rm = TRUE)) %>%
  group_by(meae_id, county) %>%
  mutate(total_vote = sum(vote, na.rm = TRUE),
         percentage = round(vote / total_vote, 3)) %>%
  select(-total_vote) %>%
  left_join(join_table, by = c("county" = "nnv_county")) %>%
  left_join(county_shp, by = c("ahcb_intermediate" = "name"))

counties_votes <- counties_tidy %>%
  select(-percentage) %>%
  mutate(party = if_else(is.na(party), "Other", party),
         party = if_else(party == "Democrat", "Republican", party)) %>%
  spread(party, vote) %>%
  rename(federalist_vote = Federalist, demrep_vote = Republican, other_vote = Other)

counties_percentage <- counties_tidy %>%
  select(-vote) %>%
  mutate(party = if_else(is.na(party), "Other", party),
         party = if_else(party == "Democrat", "Republican", party)) %>%
  spread(party, percentage) %>%
  rename(federalist_percentage = Federalist, demrep_percentage = Republican, other_percentage = Other)

counties <- left_join(counties_votes, counties_percentage,
                      by = c("county", "meae_id", "ahcb_intermediate",
                             "ahcb_county", "county_fips")) %>%
  mutate(districts = "") %>%
  rename(county_ahcb = ahcb_county) %>%
  select(meae_id, county_ahcb, county_fips, county,
         federalist_vote, federalist_percentage,
         demrep_vote, demrep_percentage,
         other_vote, other_percentage)

write_csv(candidates_total, paste0("temp/", candidates_out))
write_csv(counties, paste0("temp/", counties_out))
