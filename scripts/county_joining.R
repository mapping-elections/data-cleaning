library(magrittr)
library(readr)
library(USAboundaries)
library(stringr)
library(dplyr)

nnv <- read_tsv("data-raw/nnv-tsv/all-votes.tsv")
shpfile <- us_counties("1827-01-01", states = "New York")
all_ny <- USAboundaries::hist_us_counties@data %>% filter(state_terr == "New York") %>% distinct(name)

# Clean up variable names, filter and create distinct counties data table
names(nnv) <- names(nnv) %>%
  str_to_lower() %>%
  str_replace_all("\\.", "") %>%
  str_replace_all("\\s", "_")

ny_ce_county <- nnv %>%
  filter(office == "U.S. House of Representatives" & state == "New York" & !is.na(county))

ny_county_count <- ny_ce_county %>%
  distinct(county) %>%
  select(county, date)

# shpfile: Creating the shapefile's data table, fixing the string case, joining to distinct nnv counties
shp <-shpfile@data

lcase_name <- str_to_title(shp$name)
shp <- shp %>%
  mutate(lower_name = lcase_name)

shp_county_join <- ny_county_count %>%
  left_join(shp, by = c("county" = "lower_name"))

# all_ny: Creating the shapefile's data table, fixing the string case, joining to distinct nnv counties
lcase_name <- str_to_title(all_ny$name)
all_ny <- all_ny %>%
  mutate(lower_name = lcase_name)

all_ny_county_join <- ny_county_count %>%
  left_join(all_ny, by = c("county" = "lower_name"))

