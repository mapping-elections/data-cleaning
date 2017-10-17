library(tidyverse)
library(mappingelections)

st <- "VA"
cong <- 1
out_dir <- "~/Desktop/congress"
parties <- c("Federalist", "Anti-Federalist", "Republican")
# data("meae_congress_counties_parties")
# invisible(meae_congress_counties_parties)

counties_in_elections <- read_rds("~/dev/mapping-elections/data-cleaning/data/counties-in-elections-grouped.rds")

verify_columns <- function(df, parties = c("federalist", "antifederalist",
                                           "republican", "other"), suffix) {
  for (party in parties) {
    if (!(party %in% colnames(df))) {
      df[[party]] <- NA_integer_
    }
  }
  colnames(df)[colnames(df) %in% parties] <-
    paste0(colnames(df)[colnames(df) %in% parties], suffix)
  return(df)
}

elections <- meae_maps %>%
  filter(state == st, congress == cong) %>%
  left_join(meae_maps_to_elections, by = "meae_id") %>%
  left_join(meae_elections, by = c("election_id", "state", "congress"))

candidate_county_returns <- elections %>%
  left_join(meae_congressional_counties, by = c("election_id", "state")) %>%
  mutate(party = if_else(party %in% parties, tolower(party), "other"),
         party = if_else(party == "anti-federalist", "antifederalist", party))

party_county_returns_from_counties <- candidate_county_returns %>%
  group_by(meae_id, county_ahcb, county_fips, party) %>%
  summarize(vote = sum(vote, na.rm = TRUE)) %>%
  mutate(vote = if_else(vote == 0, NA_integer_, vote)) %>%
  group_by(meae_id, county_ahcb, county_fips) %>%
  mutate(percentage = round(vote / sum(vote, na.rm = TRUE), 3))

party_votes_from_counties <- party_county_returns_from_counties %>%
  select(meae_id, county_ahcb, county_fips, party, vote) %>%
  spread(party, vote) %>%
  verify_columns(suffix = "_vote")

party_percentages_from_counties <- party_county_returns_from_counties %>%
  select(meae_id, county_ahcb, county_fips, party, percentage) %>%
  spread(party, percentage) %>%
  verify_columns(suffix = "_percentage")

party_returns_from_counties <- party_votes_from_counties %>%
  left_join(party_percentages_from_counties,
            by = c("meae_id", "county_ahcb", "county_fips")) %>%
  mutate(districts = NA_character_,
         county_source = "county",
         checked_lam = NA, checked_jfb = NA) %>%
  select(meae_id, county_ahcb, county_fips, districts,
         federalist_vote, federalist_percentage,
         antifederalist_vote, antifederalist_percentage,
         republican_vote, republican_percentage,
         other_vote, other_percentage, county_source,
         checked_lam, checked_jfb)

county_districts <- counties_in_elections %>%
  filter(state == st, congress == cong) %>%
  mutate(districts = map_chr(districts, paste0, collapse = ", "),
         ids = map_chr(ids, paste0, collapse = ", "))

total_votes_by_party <- elections %>%
  left_join(meae_congress_candidate_totals_all, by = "election_id") %>%
  group_by(meae_id, congress, state, district, affiliation_party) %>%
  summarize(vote = sum(overview, na.rm = TRUE)) %>%
  group_by(meae_id, congress, state, district) %>%
  mutate(percentage = round(vote / sum(vote), 3))
# TODO: spread this out to match the form we expect

candidate_results <- elections %>%
  left_join(meae_congress_candidate_totals_all, by = "election_id") %>%
  left_join(meae_congbio_elected %>% filter(congbio_position == "Representative"),
            by = c("congress", "state", "district",  "candidate_id"),
            suffix = c("", "_congbio")) %>%
  mutate(winner = congbio_position == "Representative",
         winner = if_else(is.na(winner), FALSE, TRUE)) %>%
  rename(vote = overview) %>%
  group_by(election_id) %>%
  mutate(total_vote = sum(vote, na.rm = TRUE),
         percent_vote = round(vote / total_vote, 3),
         unopposed = FALSE) %>%
  select(meae_id, election_id, candidate = candidate_name, candidate_id,
         district, party = affiliation_party, vote, total_vote, percent_vote,
         winner, unopposed) %>%
  mutate(district = if_else(is.na(district), "At-large", as.character(district)))

dir.create(out_dir, showWarnings = FALSE)
basefilename <- paste0(out_dir, "/congress",
                       stringr::str_pad(cong, width = 2, pad = "0"), "-", st)

write_csv(party_returns_from_counties, paste0(basefilename, "-county-returns.csv"))
write_csv(county_districts, paste0(basefilename, "-districts2counties.csv"))
write_csv(total_votes_by_party, paste0(basefilename, "-total-returns.csv"))
write_csv(candidate_results, paste0(basefilename, "-candidate-results.csv"))
