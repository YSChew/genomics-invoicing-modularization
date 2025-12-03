source("src/server/server-helpers.R")

guess_type <- function(df) {
  ndf <- norm_names(df)
  cat_col <- c("product_category","category","type")
  cat_col <- cat_col[cat_col %in% names(ndf)]
  txt <- if (length(cat_col)) tolower(ndf[[cat_col[1]]]) else ""
  if (!length(cat_col)) {
    name_col <- c("product_name","name","brand","description")
    name_col <- name_col[name_col %in% names(ndf)]
    txt <- if (length(name_col)) tolower(ndf[[name_col[1]]]) else ""
  }
  is_processing <- str_detect(txt, paste(c(
    "process","service","analysis","sequenc","library prep","bioinform","alignment"
  ), collapse="|"))
  is_physical <- str_detect(txt, paste(c(
    "kit","chip","reagent","tube","plate","index","bead","enzyme","antibody"
  ), collapse="|"))
  tibble(
    is_item = !is.na(txt) & nzchar(txt),
    is_physical = is_item & is_physical & !is_processing,
    is_processing = is_item & is_processing
  )
}