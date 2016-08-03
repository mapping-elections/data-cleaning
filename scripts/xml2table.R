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

Usage: xml2table.R INPUT -o OUTPUT

Options:
  <INPUT>                 Path to XML record from NNV.
  -h --help               Show this message.
  -o --output <OUTPUT>    Path to output file (CSV)." -> doc

opt <- docopt(doc)

stopifnot(file.exists(opt$INPUT))
opt$output <- normalizePath(opt$output)
stopifnot(dir.exists(opt$output))

input <- read_xml(opt$INPUT)
ns <- xml_ns(input)

# Extract information which is general to every data point
office_xml <- input %>% xml_find_first(".//d1:office", ns)
office_name <- office_xml %>% xml_attr("name")
office_id <- office_xml %>% xml_attr("office_id")
election_label <- input %>% xml_attr("label")
election_type <- input %>% xml_attr("type")
election_iteration <- input %>% xml_attr("iteration")
election_date <- input %>% xml_attr("date")
election_year <- input %>% xml_attr("date") %>%
  str_extract("\\d{4}") %>%
  as.integer()
election_id <- input %>% xml_attr("election_id")

# Get the candidates' information, since elsewhere they are referred to by number
candidates_xml <- input %>%
  xml_find_first(".//d1:ballot", ns) %>%
  xml_children()

candidates <- data_frame(
  election_id = election_id,
  election_date = election_date,
  election_year = election_year,
  election_type = election_type,
  election_label = election_label,
  office_name = office_name,
  office_id = office_id,
  candidate = xml_attr(candidates_xml, "name"),
  name_id = xml_attr(candidates_xml, "name_id"),
  affiliation = xml_attr(candidates_xml, "affiliation"),
  affiliation_id = xml_attr(candidates_xml, "affiliation_id"),
  candidate_num = xml_attr(candidates_xml, "candidate_num")
)

# The overview is the summary vote total for all geographies
overview_xml <- input %>%
  xml_find_first(".//d1:overview", ns) %>%
  xml_find_all(".//d1:candidate_summary", ns)

overview <- data_frame(
  candidate_num = xml_attr(overview_xml, "candidate_ref"),
  overview_vote = xml_attr(overview_xml, "vote_total")
)

overview_summary <- candidates %>%
  left_join(overview, by = "candidate_num")

write_csv(overview_summary,
          str_c(opt$output, "/", election_id, "-overview.csv"))

# Helper
replace_null <- function(x) {
  if_else(x == "null", NA_integer_, x)
}

# This function gets the results one level deep
extract_voting <- function(node) {
  results_candidates <- node %>%
    xml_find_all("d1:result") %>%
    xml_attr("candidate_ref")
  results_vote <- node %>%
    xml_find_all("d1:result") %>%
    xml_attr("vote") %>%
    as.integer()
  results <- node %>% xml_find_all("d1:result")
  results_unit_name <- map_chr(results, function(x) {
    xml_parents(x)[[1]] %>% xml_attr("name")
    })
  results_unit_type <- node %>% xml_attr("type") %>% str_to_lower()
  # results_unit_geogid <- node %>% xml_attr("geog_id")
  # results_parent_unit_name <- xml_parents(node)[[1]] %>% xml_attr("name")

  if (all(results_unit_type == "district")) {
    df <- data_frame(
      candidate_num = results_candidates,
      district = results_unit_name,
      vote = results_vote
      )
  } else if (all(results_unit_type == "county")) {
    df <- data_frame(
      candidate_num = results_candidates,
      county = results_unit_name,
      vote = results_vote
    )
  }

  df %>%
    mutate(vote = replace_null(vote))
}

# For Congressional elections, at least, the district total should be the same
# as the overview, but we will check that assumption.
district_xml <- input %>%
  xml_find_all(".//d1:sub_unit[@type='District']")

district_summary <- candidates %>%
  left_join(extract_voting(district_xml), by = "candidate_num")

write_csv(district_summary,
          str_c(opt$output, "/", election_id, "-districts.csv"))

# Get the counties
counties_xml <- input %>%
  xml_find_all(".//d1:sub_unit[@type='County']")

# Make sure we include NAs for candidates even when they didn't get votes in
# particular locations
county_votes <- extract_voting(counties_xml)
na_grid <- expand.grid(candidate_num = candidates$candidate_num,
                       county = unique(county_votes$county),
                       stringsAsFactors = FALSE)

county_summary <- na_grid %>%
  left_join(candidates, by = "candidate_num") %>%
  left_join(county_votes, by = c("candidate_num", "county"))

write_csv(county_summary,
          str_c(opt$output, "/", election_id, "-counties.csv"))
