library(magrittr)
library(readr)
library(USAboundaries)
library(stringr)
library(dplyr)

nnv <- read_tsv("data-raw/nnv-tsv/all-votes.tsv")
all_ny <- USAboundaries::hist_us_counties@data %>% filter(state_terr == "New York")
troublesome_elections <- read_csv("data/troublesome_elections.csv")
county_join_table <- read_csv("data/county_join_table.csv") %>%
  select(nnv_county, ahcb_county)

# Clean up variable names, filter and create distinct counties data table
names(nnv) <- names(nnv) %>%
  str_to_lower() %>%
  str_replace_all("\\.", "") %>%
  str_replace_all("\\s", "_")

ny_ce_county <- nnv %>%
  filter(office == "U.S. House of Representatives" & state == "New York" & !is.na(county)) %>%
  filter(!id %in% troublesome_elections$election_id) %>%
  count(county)

# all_ny: Creating the shapefile's data table, fixing the string case, joining to distinct nnv counties
tcase_name <- str_to_title(all_ny$name)
all_ny <- all_ny %>%
  mutate(title_case = tcase_name)

ce_join <- ny_ce_county %>%
  left_join(county_join_table, by = c("county" = "nnv_county"))

ahcb_nnv_join <- ce_join %>%
  left_join(all_ny, by = c("ahcb_county" = "name"))
