library(tidyverse)
library(mappingelections)

# This script takes the affiliations from `congressional-candidates-totals.csv`
# and updates `congressional-counties.csv` with that data. This script uses the
# data which is already in the mappingelections package. So make sure that
# package is up to date with the most recent versions of the data.

aff <- meae_congress_candidate_totals %>%
  select(election_id, candidate_id, party) %>%
  filter(!is.na(candidate_id))

correct_parties <- function(old, new) {
  ifelse(!is.na(new), new, old)
}

new_with_aff <- meae_congressional_counties %>%
  left_join(aff, by = c("election_id", "candidate_id"), suffix = c("", "_new")) %>%
  mutate(party = correct_parties(party, party_new)) %>%
  select(-party_new)

nrow(new_with_aff) - nrow(meae_congressional_counties)
stopifnot(nrow(meae_congressional_counties) == nrow(new_with_aff))

write_csv(new_with_aff, "congressional-counties.csv")

