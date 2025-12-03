source("src/server/server-helpers.R")

parse_data <- function(df) {
  target_col <- "Additional reagent Cost (not incl. in kit)"
  if (target_col %in% names(df)) {
    df[[target_col]][is.na(df[[target_col]])] <- 0
  }
  df
}

clean_invoice_cols <- function(df) {
  need <- c("per reaction cost", "%PRJ surcharge", "%EXTERNAL surcharge",
            "Additional reagent Cost (not incl. in kit)")
  for (nm in need) if (!nm %in% names(df)) df[[nm]] <- 0
  df[["per reaction cost"]] <- to_num(df[["per reaction cost"]])
  df[["%PRJ surcharge"]] <- to_num(df[["%PRJ surcharge"]])
  df[["%EXTERNAL surcharge"]] <- to_num(df[["%EXTERNAL surcharge"]])
  df[["Additional reagent Cost (not incl. in kit)"]] <- to_num(df[["Additional reagent Cost (not incl. in kit)"]])
  for (nm in need) df[[nm]][is.na(df[[nm]])] <- 0
  df
}