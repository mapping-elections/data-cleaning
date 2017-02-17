library(stringr)
data_subset <-  data %>% filter(election_id == "ny.uscongress5.1789")

prepare_for_mapping <- function(df) {
  df %>%
    select(county, election_id, election_year, candidate, vote) %>%
    spread(candidate, vote, fill = 0)
}

prepare_for_mapping(data_subset)

?split

data_split <- data %>% split(data$election_id)

try_prepare_for_mapping <- failwith(NULL, prepare_for_mapping)


data_split2 <- data_split %>%
  map(try_prepare_for_mapping) %>%
  discard(is.null)

write_to_disk <- function(id) {
  df <- data_split2[[id]]
  write_csv(df, str_c(id, "-for-mapping.csv"))
}

names(data_split2) %>%
  walk(write_to_disk)
