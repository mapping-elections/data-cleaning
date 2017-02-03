# Interactive script to call other scripts for exporting individual data,
# then checking it
library(readr)
library(dplyr)
library(stringr)
library(purrr)

# Set the state you are working on here
STATE <- "RI"

# Export individual elections
elections <- read_csv("data/congressional-elections-list.csv")

# Adjust this to get just the elections you need
keepers <- elections %>%
  filter(!special,
         state == STATE,
         !str_detect(id, "nomination")) %>%
  arrange(state, year, district)
         # str_detect(id, "uscongress"),
         # !str_detect(id, "spring"))

files <- keepers$filename

run_export <- function(f) {
  cmd <- str_c("./scripts/xml2table.R data-raw/nnv-xml/", f,
               " -o data/congressional-individual/", STATE)
  message(cmd)
  system(cmd)
}

dir.create(str_c("data/congressional-individual/", STATE), showWarnings = FALSE)
walk(files, run_export)

# Create the files that do the checking
run_check <- function(df) {
  cmd <- str_c("./scripts/check-election-aggregates.R ",
               " --input ", "data/congressional-individual/", STATE,
               " --year ", df$year,
               " --state ", df$state,
               " --output ", "data/congressional-county-checks/", STATE,
               sep = "")
  message(cmd)
  system(cmd)
}

keepers %>%
  distinct(state, year) %>%
  by_row(run_check)
