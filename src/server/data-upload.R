library(readxl)
source("src/server/server-helpers.R")

parse_data <- function(df) {
  target_col <- "Additional reagent Cost (not incl. in kit)"
  if (target_col %in% names(df)) {
    df[[target_col]][is.na(df[[target_col]])] <- 0
  }
  df
}

#rv stands for reactiveVal
convert_spreadsheet_to_df <- function(filepath, raw_data_rv, processed_data_rv, meta_info) {
  if (!is.null(filepath)) {
    df <- read_excel(filepath)
    raw_data_rv(df)
    processed_data_rv(parse_data(df))
    meta_info(list(date = as.character(Sys.Date()), version = "N/A"))
  } else {
    showNotification("Please upload a file first.", type = "warning")
  }
}

verify_upload <- function(input, file_path_rv, raw_data_rv, 
                          processed_data_rv, invoice_items_data_rv) {
  req(input$file)
  file_path_rv(input$file$datapath)
  raw_data_rv(NULL)
  processed_data_rv(NULL)
  invoice_items_data_rv(NULL)
}