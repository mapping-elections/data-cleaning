# NNV uses "null" when they mean NA
strip_null <- function(x) {
  stopifnot(is.character(x))
  ifelse(x == "null", NA_character_, x)
}

most_common_in_vector <- function(x) {
  res <- x %>% table() %>% sort(decreasing = TRUE) %>% names()
  res[1]
}

reverse_name <- function(x) {
  requireNamespace("humaniformat")
  requireNamespace("stringr")
  stopifnot(is.character(x))
  parsed <- humaniformat::parse_names(x)
  parsed$middle_name <- ifelse(is.na(parsed$middle_name), "", parsed$middle_name)
  res <- stringr::str_c(parsed$last_name, ", ", parsed$first_name,
                        " ", parsed$middle_name)
  stringr::str_trim(res)
}
