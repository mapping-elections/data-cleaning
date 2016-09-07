library(dplyr)
opt <- list(INPUT = "~/dev/mapping-elections/data-cleaning/data-raw/nnv-xml/ny.uscongress5.1789.xml",
            output = "~/Desktop/")

nnv <- readr::read_tsv("data-raw/nnv-tsv/all-votes.tsv")
names(nnv) <- tolower(names(nnv))

# 1789 district 1 has only one candidate
# 1789 district 2 has New York city with different wards
# 1789 district 3 has two candidates with counties without votes and no towns
# 1789 district 4 has only one candidate
# 1789 district 5 has many candidates with votes at both towns and counties
test_election <- nnv %>%
  filter(state == "New York",
         office == "U.S. House of Representatives",
         id == "ny.uscongress5.1789")
