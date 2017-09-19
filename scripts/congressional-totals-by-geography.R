# We need to get the Congressional totals by geography and candidates to fill in
# missing data

library(tidyverse)
library(mappingelections)
library(stringr)

elections <- meae_maps %>%
  filter(geography == "county") %>%
  left_join(meae_maps_to_elections, by = "meae_id")

files <- str_c("data-raw/nnv-xml/", elections$election_id, ".xml")

parse_xml_safely <- safely(parse_nnv_xml)

parsed_files <- map(files, parse_xml_safely) %>%
  transpose()

errors <- parsed_files$error
names(errors) <- files
errors %>% keep(negate(is.null))

vote_totals <- parsed_files$result %>% bind_rows()

write_csv(vote_totals, "../elections-data/congressional-candidate-totals.csv")
