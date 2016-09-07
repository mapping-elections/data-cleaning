library(dplyr)
library(readr)
library(USAboundaries)

ny_county_votes <- read_csv("data/congressional/ny-congressional-counties.csv")
join_codes <- read_csv("data/join_tables/ny_county_join_table.csv")

# Gathering relevant AHCB data and correcting id discrepancies
# (check for repeated ids and then filter them out)
ny_county_codes <- hist_us_counties@data %>%
  filter(state_terr == "New York") %>%
  select(name, id, fips) %>%
  filter(name != "TRYON", name != "CHARLOTTE") %>%
  distinct(name, id, fips)

# Joining the NNV data to join table, removing notes variable, and joining to AHCB data
ny_joined <- ny_county_votes %>%
  left_join(join_codes, by = c("county" = "nnv_county")) %>%
  select(-notes) %>%
  left_join(ny_county_codes, by = c("ahcb_county" = "name"))

# throws error message if duplicate NNV rows created during join
stopifnot(nrow(ny_joined) == nrow(ny_county_votes))

# throws error message if any NA values exist in fips variable
stopifnot(ny_joined %>% filter(is.na(fips)) %>% nrow() == 0)

write_csv(ny_joined, "data/congressional/ny-congressional-counties-codes.csv")
