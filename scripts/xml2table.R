#!/usr/bin/env Rscript
#
# Mapping Early American Elections

suppressMessages(library(docopt))
suppressMessages(library(dplyr))
suppressMessages(library(purrr))
suppressMessages(library(stringr))
suppressMessages(library(xml2))
suppressMessages(library(readr))

"Convert XML election records from NNV to tabular data.

Usage: xml2table.R [-u UNIT] INPUT -o OUTPUT

Options:
  <INPUT>                 Path to XML record from NNV.
  -h --help               Show this message.
  -o --output <OUTPUT>    Path to output file (CSV).
  -u --unit <UNIT>        The geographical unit to return [default: town]." -> doc

opt <- docopt(doc)

opt$unit <- str_to_title(opt$unit)
possible_units <- c("Town")
if (!opt$unit %in% possible_units) {
  warning("Geographical unit must be one of ", str_c(possible_units, collapse = ", "))
  quit(status = 1)
}

stopifnot(file.exists(opt$INPUT))
input <- read_xml(opt$INPUT)
ns <- xml_ns(input)

general_xml <- input %>% xml_find_one(".//d1:office", ns)
office_name <- general_xml %>% xml_attr("name")
office_id <- general_xml %>% xml_attr("office_id")

candidates_xml <- input %>%
  xml_find_one(".//d1:ballot", ns) %>%
  xml_children()

candidates <- data_frame(
  candidate = xml_attr(candidates_xml, "name"),
  name_id = xml_attr(candidates_xml, "name_id"),
  affiliation = xml_attr(candidates_xml, "affiliation"),
  affiliation_id = xml_attr(candidates_xml, "affiliation_id"),
  candidate_num = xml_attr(candidates_xml, "candidate_num")
)

overview_xml <- input %>%
  xml_find_one(".//d1:overview", ns) %>%
  xml_find_all(".//d1:candidate_summary", ns)

overview <- data_frame(
  candidate_num = xml_attr(overview_xml, "candidate_ref"),
  overview_vote_total = xml_attr(overview_xml, "vote_total")
)

sub_units_xml <- input %>%
  xml_find_all(str_c(".//d1:sub_unit[@type='", opt$unit, "']"), ns)

extract_voting <- function(node) {
  results_candidates <- node %>% xml_children() %>% xml_attr("candidate_ref")
  results_vote <- node %>% xml_children() %>% xml_attr("vote") %>% as.integer()
  results_unit_name <- node %>% xml_attr("name")
  # results_unit_type <- node %>% xml_attr("type")
  results_unit_geogid <- node %>% xml_attr("geog_id")
  results_parent_unit_name <- xml_parents(node)[[1]] %>% xml_attr("name")

  data_frame(candidate_num = results_candidates,
             vote = results_vote,
             town = results_unit_name,
             county = results_parent_unit_name,
             town_full = str_c(town, ", ", county, " County"),
             geog_id = results_unit_geogid)
}

votes <- sub_units_xml %>%
  as_list() %>%
  map(extract_voting) %>%
  bind_rows()

replace_null <- function(x) {
  ifelse(x == "null", NA, x)
}

# Make sure we include NAs for candidates even when they didn't get votes in
# particular locations
output <- expand.grid(candidate_num = candidates$candidate_num,
                      town_full = unique(votes$town_full),
                      stringsAsFactors = FALSE) %>%
  left_join(votes, by = c("candidate_num", "town_full")) %>%
  left_join(candidates, by = "candidate_num") %>%
  left_join(overview, by = "candidate_num") %>%
  dmap_if(is.character, replace_null) %>%
  mutate(office_name = office_name,
         office_id = office_id,
         election_id = str_replace(basename(opt$INPUT), ".xml", "")) %>%
  select(election_id, office_name, office_id,
         candidate, candidate_id = name_id,
         affiliation, affiliation_id,
         town, county, town_full,
         vote, overview_vote_total)

write_csv(output, opt$output)
