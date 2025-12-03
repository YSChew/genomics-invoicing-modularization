to_num <- function(x) {
  if (is.list(x)) x <- vapply(x, function(y) if (length(y)) y[[1]] else NA_character_, character(1))
  x <- as.character(x)
  x <- gsub(",", "", x)
  x <- gsub("%", "", x)
  x <- gsub("\\$", "", x)
  suppressWarnings(as.numeric(x))
}

norm_names <- function(df) {
  names(df) <- names(df) |>
    tolower() |>
    gsub("\\s+", "_", x = _, perl = TRUE)
  df
}