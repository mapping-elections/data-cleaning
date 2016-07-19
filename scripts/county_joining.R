library(magrittr)
library(readr)
library(USAboundaries)
library(stringr)
library(dplyr)

# Need to edit the state inputs according to the state of interest
STATE <- "Virginia"
JOIN_TABLE <- "data/va_county_join_table.csv"

nnv <- read_tsv("data-raw/nnv-tsv/all-votes.tsv")
county_shp <- USAboundaries::hist_us_counties@data %>% filter(state_terr == STATE) %>% distinct()
troublesome_elections <- read_csv("data/troublesome_elections.csv")

# Clean up variable names, filter and create distinct counties data table
names(nnv) <- names(nnv) %>%
  str_to_lower() %>%
  str_replace_all("\\.", "") %>%
  str_replace_all("\\s", "_")

cong_elect_county <- nnv %>%
  filter(office == "U.S. House of Representatives" & state == STATE & !is.na(county)) %>%
  filter(!id %in% troublesome_elections$election_id) %>%
  count(county)

# county_bound: Creating the shapefile's data table, fixing the string case, joining to distinct nnv counties
county_bound <- county_shp %>%
  distinct(name)

tcase_name <- str_to_title(county_bound$name)
county_bound <- county_bound %>%
  mutate(title_case = tcase_name)

ce_county_left_join <- cong_elect_county %>%
  left_join(county_bound, by = c("county" = "title_case"))

# Create the county joining .csv file
#write_csv(ce_county_left_join, "va_county_join.csv")



#Joining the two datatables by the county-join-table
county_join_table <- read_csv(JOIN_TABLE)

cong_elect_join <- cong_elect_county %>%
  left_join(county_join_table, by = c("county" = "nnv_county"))

ahcb_nnv_join <- cong_elect_join %>%
  left_join(county_bound, by = c("ahcb_county" = "name"))
