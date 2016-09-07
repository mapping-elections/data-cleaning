#!/usr/bin/env Rscript --vanilla
#
# List congressional elections in a CSV file

library(dplyr)
library(stringr)
library(readr)
ids <- dir("data-raw/nnv-xml/")

congressional <- tibble(filename = ids) %>%
  filter(str_detect(filename, "congress")) %>%
  mutate(id = str_replace(filename, "\\.xml", ""),
         year = as.integer(str_extract(filename, "\\d{4}")),
         state = str_to_upper(str_extract(filename, "^\\w{2}")),
         special = str_detect(filename, "special"),
         district = str_extract(filename, "\\d+\\.") %>%
           str_replace("\\.", "") %>%
           as.integer()) %>%
  mutate(district = if_else(district > 1780, NA_integer_, district)) %>%
  select(state, year, id, filename, everything()) %>%
  arrange(state, year, district, id)

write_csv(congressional, "data/congressional-elections-list.csv")
