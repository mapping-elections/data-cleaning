# Temporary script to call other scripts for exporting individual data,
# then checking it
library(readr)
library(dplyr)
library(stringr)
library(purrr)

# Export individual elections
elections <- read_csv("data/congressional-elections-list.csv")

keepers <- elections %>%
  filter(!special,
         state == "NY",
         str_detect(id, "uscongress"),
         !str_detect(id, "spring"))

files <- keepers$filename

run_export <- function(f) {
  cmd <- str_c("./scripts/xml2table.R data-raw/nnv-xml/", f,
               " -o data/congressional-individual/NY")
  message(cmd)
  system(cmd)
}

walk(files, run_export)

# Create the files that do the checking
run_check <- function(df) {
  cmd <- str_c("./scripts/check-election-aggregates.R",
               "--input", "data/congressional-individual/NY",
               "--year", df$year,
               "--state", df$state,
               "--output", "data/congressional-county-checks/NY",
               sep = " ")
  message(cmd)
  system(cmd)
}

keepers %>%
  distinct(state, year) %>%
  by_row(run_check)

