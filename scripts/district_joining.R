library(magrittr)
library(readr)
library(USAboundaries)
library(stringr)
library(dplyr)
library(foreign)
library(purrr)

nnv <- read_tsv("data-raw/nnv-tsv/all-votes.tsv")

congress_df <- Sys.glob("data-raw/congressional-shp/*.dbf") %>%
  map(read.dbf) %>%
  bind_rows()

# Clean up variable names, filter and create distinct counties data table
names(nnv) <- names(nnv) %>%
  str_to_lower() %>%
  str_replace_all("\\.", "") %>%
  str_replace_all("\\s", "_")

cong_elect_district <- nnv %>%
  filter(office == "U.S. House of Representatives" & state == "Virginia" & !is.na(district))

names(congress_df) <- names(congress_df) %>%
  str_to_lower() %>%
  str_replace_all("\\.", "") %>%
  str_replace_all("\\s", "_")

district_df <- congress_df %>%
  filter(statename == "Virginia") %>%
  distinct(id, startcong, endcong, district, .keep_all = TRUE)

# Writes .csv of distinct congressional election ids to create the joining table
# cong_elect_district %>%
#   select(id, date, district) %>%
#   distinct(id, .keep_all = TRUE) %>%
#   write_csv("va_district_join_table.csv")



#Joining the two datatables by the county-join-table
district_join_table <- read_csv("data/va_district_join_table.csv")

cong_elect_join <- cong_elect_district %>%
  left_join(district_join_table, by = c("id" = "nnv_id"))

ahcb_nnv_join <- cong_elect_join %>%
  left_join(district_df, by = c("congressional_id" = "id"))

