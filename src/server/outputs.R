generateQuoteID <- function() {
  date_part <- format(Sys.Date(), "%Y%m%d")
  random_part <- sprintf("%04d", sample(0:9999, 1))
  paste0("WEHI-AGF-", date_part, "-", random_part)
}