# NNV uses "null" when they mean NA
strip_null <- function(x) {
  stopifnot(is.character(x))
  ifelse(x == "null", NA_character_, x)
}
