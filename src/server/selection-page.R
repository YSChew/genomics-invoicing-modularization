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

# This function is used to filter the data based on the brand or product selected
filter_data <- function(input, processed_data){
  req(processed_data())
  df <- processed_data()
  if (!is.null(input$brand_filter) && length(input$brand_filter) > 0) {
    df <- df[df$Brand %in% input$brand_filter, , drop = FALSE]
  }
  if (!is.null(input$product_filter) && length(input$product_filter) > 0) {
    df <- df[df$`Product Name` %in% input$product_filter, , drop = FALSE]
  }
  df
}

populate_brand_product_filters <- function(processed_data, session) {
  df <- processed_data()
  if (is.null(df)) return()
  if ("Brand" %in% names(df)) {
    updateSelectizeInput(session, "brand_filter",
                         choices = sort(unique(df$Brand)), server = TRUE)
  }
  if ("Product Name" %in% names(df)) {
    updateSelectizeInput(session, "product_filter",
                         choices = sort(unique(df$`Product Name`)), server = TRUE)
  }
}

select_rows <- function(input, output, session, filtered_data) {
  rows <- input$data_table_rows_selected
  df <- filtered_data()
  if (!is.null(df) && length(rows) > 0) {
    return(df[rows, , drop = FALSE])
  } else {
    return(NULL)
  }
}

generate_master_summary_df <- function(processed_data) {
  req(processed_data())
  df <- processed_data()
  meta_info <- reactiveVal(list(date = as.character(Sys.Date()), version = "N/A"))
  flags <- guess_type(df)
  
  return(data.frame(
    Date = meta_info()$date,
    Version = meta_info()$version,
    `Total items` = sum(flags$is_item, na.rm = TRUE),
    `Physical items` = sum(flags$is_physical, na.rm = TRUE),
    `Processing items` = sum(flags$is_processing, na.rm = TRUE),
    check.names = FALSE
  ))
}

generateInvoiceTable <- function(invoice_items_data) {
  req(invoice_items_data)
  
  items <- invoice_items_data
  
  items$Quantity <- 1
  
  
  items$Amount <- as.numeric(items$`%PRJ surcharge`)
  items$Total <- items$Quantity * items$Amount
  items$Description <- paste(items$Brand, items$`Product Category`, sep = " - ")
  
  formatted <- items[, c("Product Name", "Description", "Quantity", "Amount", "Total")]
  colnames(formatted) <- c("Item", "Description", "Quantity", "Amount", "Total")
  
  return(formatted)
}