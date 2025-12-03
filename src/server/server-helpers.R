to_num <- function(x) {
  if (is.list(x)) x <- vapply(x, function(y) if (length(y)) y[[1]] else NA_character_, character(1))
  x <- as.character(x)
  x <- gsub(",", "", x)
  x <- gsub("%", "", x)
  x <- gsub("\\$", "", x)
  suppressWarnings(as.numeric(x))
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

norm_names <- function(df) {
  names(df) <- names(df) |>
    tolower() |>
    gsub("\\s+", "_", x = _, perl = TRUE)
  df
}

report_params_list <- function(input, pdf_table_data) {
  return(params <- list(
    date = Sys.Date(),
    quote_id = input$quote_id,
    project_id = input$project_id,
    project_title = input$project_title,
    project_type = input$project_type,
    platform = input$platform,
    table_data = pdf_table_data
  ))
}

verify_empty_df <- function(compare_df) {
  return(identical(compare_df, data.frame(Item=character(0), Description=character(0),
                                          Quantity=numeric(0), Amount=numeric(0), Total=numeric(0))))
}

clean_invoice_data <- function(invoice_items_data) {
  # 1st filter, check null or non-empty
  dat <- invoice_items_data()
  if (is.null(dat) || nrow(dat) == 0) {
    return(data.frame(Item=character(0), Description=character(0),
                      Quantity=numeric(0), Amount=numeric(0), Total=numeric(0)))
  }
  
  # 2nd filter, further clean df and check null after cleaning
  dat2 <- tryCatch(clean_invoice_cols(dat),
                   error = function(e) {
                     showNotification(paste("Invoice data is malformed:", e$message),
                                      type = "error", duration = NULL)
                     return(NULL)
                   })
  if (is.null(dat2)) {
    return(data.frame(Item=character(0), Description=character(0),
                      Quantity=numeric(0), Amount=numeric(0), Total=numeric(0)))
  }
  
  # cleaning complete, attempt to convert into invoiceTable
  tryCatch(
    generateInvoiceTable(dat2),
    error = function(e) {
      showNotification(paste("Failed to build invoice table:", e$message),
                       type = "error", duration = NULL)
      data.frame(Item=character(0), Description=character(0),
                 Quantity=numeric(0), Amount=numeric(0), Total=numeric(0))
    }
  )
}