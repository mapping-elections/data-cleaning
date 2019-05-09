library(tidyverse)
files <- Sys.glob("towns/*.csv")

walk(files, function(f) {
  read_csv(f, col_types = "ccdd") %>%
    distinct(town, .keep_all = TRUE) %>%
    arrange(town) %>%
    write_csv(f)
})
