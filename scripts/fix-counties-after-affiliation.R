#!/usr/bin/env Rscript

suppressPackageStartupMessages(library(docopt))
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(mappingelections))

"Update the county party totals after fixing the affiliations.

Usage: fix-counties-after-affiliation.R --map <map>" -> doc

opt <- docopt(doc)
# opt <- list(map = "meae.congressional.congress01.de.county")

votes_tidy <- meae_maps %>%
  filter(meae_id == opt$map) %>%
  left_join(meae_maps_to_elections, by = "meae_id") %>%
  left_join(meae_congressional_counties, by = "election_id") %>%
  mutate(party = ifelse(is.na(party), "Other", party)) %>%
  group_by(county_ahcb, party) %>%
  summarize(vote = sum(vote)) %>%
  group_by(county_ahcb) %>%
  mutate(total_vote = sum(vote)) %>%
  mutate(percentage = round(vote / total_vote, 3))

rename_cols <- function(df, append) {
  colnames(df) <- ifelse(colnames(df) %in% c("county_ahcb", "party"),
                         colnames(df),
                         paste0(colnames(df), append))
  df
}

votes <- votes_tidy %>%
  select(county_ahcb, party, vote) %>%
  spread(party, vote) %>%
  rename_cols("_vote")

percentages <- votes_tidy %>%
  select(county_ahcb, party, percentage) %>%
  spread(party, percentage) %>%
  rename_cols("_percentage")

total_votes <- votes_tidy %>%
  distinct(county_ahcb, total_vote)

out <- votes %>%
  left_join(percentages, by = "county_ahcb") %>%
  left_join(total_votes, by = "county_ahcb")

write_csv(out, "~/Desktop/fixes-to-parties.csv")
