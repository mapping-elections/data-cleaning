# Make sure that elections.csv in the elections-data repository has all the
# Congressional elections from NNV.

library(tidyverse)
library(stringr)

# Using the file in the elections-data repository because it has been edited
elections <- read_csv("../elections-data/elections.csv")

# Get all the NNV ids
nnv_files <- Sys.glob("data-raw/nnv-xml/*.xml") %>%
  basename() %>%
  str_replace("\\.xml$", "")
nnv_ids <- tibble(election_id = nnv_files) %>%
  arrange(election_id)

# Get just the NNV Congressional files
nnv_ids <- nnv_ids %>%
  filter(str_detect(election_id, "congress")) %>%
  filter(!str_detect(election_id, "continentalcongress")) %>%
  filter(!str_detect(election_id, "nomination"))

# Elections to add and delete
to_add <- anti_join(nnv_ids, elections, by = "election_id")

to_delete <- anti_join(elections, nnv_ids, by = "election_id") %>%
  select(election_id)

deleted <- to_delete %>%
  left_join(elections, by = "election_id")

write_csv(deleted, "data/deleted-from-elections.csv")


elections_fixed <- elections %>%
  anti_join(to_delete, by = "election_id") %>%
  bind_rows(to_add) %>%
  arrange(!is.na(election_office)) %>%
  mutate(election_office = if_else(is.na(election_office), "U.S. House of Representatives", election_office)) %>%
  mutate(state = if_else(is.na(state), str_sub(election_id, 1, 2) %>% str_to_upper(), state)) %>%
  mutate(year = if_else(is.na(year), str_extract(election_id, "\\d{4}") %>% as.integer(), year)) %>%
  arrange(!is.na(election_type)) %>%
  mutate(election_type = if_else(is.na(election_type) & str_detect(election_id, "(special)|(vacancy)"), "Special", election_type)) %>%
  mutate(election_type = if_else(is.na(election_type), "General", election_type)) %>%
  arrange(state, congress, district)

write_csv(elections_fixed, "../elections-data/elections.csv")
