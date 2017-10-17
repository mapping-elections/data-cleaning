library(tidyverse)
library(mappingelections)
library(stringr)
library(xml2)

elections <- meae_maps %>%
  filter(geography == "county") %>%
  left_join(meae_maps_to_elections, by = "meae_id")

files <- str_c("~/dev/mapping-elections/data-cleaning/data-raw/nnv-xml/",
               elections$election_id, ".xml")

get_counties_by_election <- function(f) {
  nnv_xml <- read_xml(f)
  ns <- xml_ns(nnv_xml)
  id <- f %>% basename() %>% str_replace("\\.xml$", "")

  units <- nnv_xml %>%
    xml_find_all(".//d1:sub_unit", ns)

  geo_type <- units %>% xml_attr("type")
  geo_name <- units %>% xml_attr("name")

  if (length(geo_type) == 0 & length(geo_name) == 0) return(NULL)

  data_frame(nnv_id = id, geo_type, county = geo_name) %>%
    filter(geo_type == "County") %>%
    select(-geo_type)
}

get_counties <- possibly(get_counties_by_election, NULL)
parsed <- files %>% map(get_counties) %>% discard(is.null) %>% bind_rows()

counties_in_elections <- parsed %>%
  left_join(meae_elections, by = c("nnv_id" = "election_id")) %>%
  arrange(congress, state)

write_csv(counties_in_elections,  "data/counties-in-elections.csv")

counties_in_elections_grouped <- counties_in_elections %>%
  # filter(state == "NY", congress == 1) %>%
  group_by(county, state, congress) %>%
  summarize(ids = list(nnv_id), districts = list(district)) %>%
  arrange(congress, state)

write_rds(counties_in_elections_grouped, "data/counties-in-elections-grouped.rds")

# join_table <- Sys.glob("data/join_tables/county/*.csv") %>%
#   map_df(read_csv) %>%
#   select(-notes)

