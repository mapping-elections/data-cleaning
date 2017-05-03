library(dplyr)
library(readr)
library(stringr)

NNV <- read_tsv("data-raw/nnv-tsv/all-votes.tsv")
congress_dates <- read_csv("data/join_tables/district/congress_year_table.csv")

STATE <- "Pennsylvania"
ST_ABBR <- "PA"

names(NNV) <- names(NNV) %>%
  str_to_lower() %>%
  str_replace_all("\\.", "") %>%
  str_replace_all("\\s", "_")

#Filter NNV down to both specific state and congressional election's ids
congress_table <- NNV %>%
  filter(state == STATE | state == ST_ABBR,
         office == "U.S. House of Representatives") %>%
  distinct(id)

#Extract the state abbreviation
congress_table <- congress_table %>%
  mutate(state = str_extract(id, "(..)") %>% toupper())

#Extract the election year
congress_table <- congress_table %>%
mutate(year = str_extract(id, "\\d{4}") %>%
         as.integer())

#Join to congress_dates table to create congress column
congress_table <- congress_table %>%
  left_join(congress_dates, by = c("year" = "year"))

#extract congressional district
congress_table <- congress_table %>%
  mutate(district = str_extract(id, "\\d+") %>%
  as.integer())

write_csv(congress_table, create_intermediate_filename(ST_ABBR))

create_intermediate_filename <- function(state_abbr) {
  path_to_output <- "data/join_tables/district/"
  file_suffix <- "_intermediate.csv"
  str_c(path_to_output, str_to_lower(state_abbr), file_suffix)
}


# congress_number <- function(elect_year){
#   ifelse(elect_year == 1788 | elect_year == 1789, 1,
#   ifelse(elect_year == 1790 | elect_year == 1791, 2,
#   ifelse(elect_year == 1792 | elect_year == 1793, 3,
#   ifelse(elect_year == 1794 | elect_year == 1795, 4,
#   ifelse(elect_year == 1796 | elect_year == 1797, 5,
#   ifelse(elect_year == 1798 | elect_year == 1799, 6,
#   ifelse(elect_year == 1800 | elect_year == 1801, 7,
#   ifelse(elect_year == 1802 | elect_year == 1803, 8,
#   ifelse(elect_year == 1804 | elect_year == 1805, 9,
#   ifelse(elect_year == 1806 | elect_year == 1807, 10,
#   ifelse(elect_year == 1808 | elect_year == 1809, 11,
#   ifelse(elect_year == 1810 | elect_year == 1811, 12,
#   ifelse(elect_year == 1812 | elect_year == 1813, 13,
#   ifelse(elect_year == 1814 | elect_year == 1815, 14,
#   ifelse(elect_year == 1816 | elect_year == 1817, 15,
#   ifelse(elect_year == 1818 | elect_year == 1819, 16,
#   ifelse(elect_year == 1820 | elect_year == 1821, 17,
#   ifelse(elect_year == 1822 | elect_year == 1823, 18,
#   ifelse(elect_year == 1824 | elect_year == 1825, 19,
#   ifelse(elect_year == 1826 | elect_year == 1827, 20,
#          elect_year))))))))))))))))))))
# }
#
