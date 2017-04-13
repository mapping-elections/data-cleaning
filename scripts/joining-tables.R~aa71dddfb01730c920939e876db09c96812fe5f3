library(tidyverse)
library(stringr)

data <- read_csv("data/congressional/ny-congressional-counties.csv")
join_table <- read_csv("data/join_tables/ny_county_join_table.csv")
join_table <- join_table %>% select(-notes)

data_join <- data %>%
  left_join(join_table, by = c("county" = "nnv_county"))

data_subset <- data_join %>% filter(election_id == "ny.uscongress1.1802")

prepare_for_mapping <- function(df) {
  df %>%
    select(county, election_id, election_year, candidate, vote, ahcb_county) %>%
    spread(candidate, vote, fill = 0)
}
try_prepare_for_mapping <- failwith(NULL, prepare_for_mapping)

try_prepare_for_mapping(data_subset)


data_split <- data_join %>% split(data$election_id)

data_split2 <- data_split %>%
  map(try_prepare_for_mapping) %>%
  discard(is.null)

write_to_disk <- function(id) {
  df <- data_split2[[id]]
  write_csv(df, str_c(id, "-for-mapping.csv"))
  }

names(data_split2) %>%
  walk(write_to_disk)

###Party Split
data %>% filter(election_date == 1808) %>%
  group_by(county, affiliation) %>%
  summarize(vote = sum(vote, na.rm = TRUE)) %>%
  filter(affiliation != "null") %>%
  group_by(county) %>%
  mutate(percentage = vote / sum(vote), percentage_diff = round(percentage - 0.5, 2)) %>%
  filter(affiliation == "Republican") %>%
  left_join(join_table, by = c("county" = "nnv_county")) %>%
  write_csv("mapping-test/party-election/ny-1808-winners.csv")
