# Temporary script to call other script on
library(readr)
library(dplyr)
library(stringr)
library(purrr)

elections <- read_csv("data/congressional-elections-list.csv")

files <- elections %>%
  filter(!special,
         state == "NY",
         str_detect(id, "uscongress"),
         !str_detect(id, "spring")) %>%
  `$`("filename")

run_export <- function(f) {
  cmd <- str_c("./scripts/xml2table.R data-raw/nnv-xml/", f,
               " -o data/congressional-individual/NY")
  system(cmd)
}

walk(files[1], run_export)
