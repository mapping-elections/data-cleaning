# Get all the candidates and candidate IDs from NNV, not just the Congressional.

library(tidyverse)
library(xml2)
source("R/helpers.R")

# Create the candidate records from the candidate authority files
names_dir <- "data-raw/nnv-names/candidate"
name_authorities <- list.files(names_dir, full.names = TRUE, pattern = "\\.xml")

parse_name_auth <- function(x) {
  candidate_name <- x %>%
    xml_find_all("auth:candidate") %>%
    xml_attr("name")
  candidate_id <- x %>%
    xml_find_all("auth:candidate") %>%
    xml_attr("id")
  tibble(candidate_id, candidate_name)
}

candidates <- name_authorities %>%
  map(read_xml) %>%
  map_df(parse_name_auth) %>%
  arrange(candidate_id)

write_csv(candidates, "data/name-authorities.csv")
