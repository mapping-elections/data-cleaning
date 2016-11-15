# Create the name table from the XML authority records for names
library(tidyverse)
library(xml2)

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
  arrange(candidate_id) %>%
  filter(candidate_id != "AA0000")
