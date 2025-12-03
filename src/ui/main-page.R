render_main_page <- function() {
  fluidPage(
    sidebarLayout(
      sidebarPanel(
        fileInput("file", "Upload Master Spreadsheet (.xlsx)", accept = ".xlsx"),
        actionButton("upload_button", "Upload Master Spreadsheet", class = "upload-button"),
        br(),
        h4("Filter items"),
        selectizeInput("brand_filter", "Brand", choices = NULL, multiple = TRUE,
                       options = list(placeholder = "All brands")),
        selectizeInput("product_filter", "Product", choices = NULL, multiple = TRUE,
                       options = list(placeholder = "All products")),
        br(),
        actionButton("create_invoice_page", "Create Invoice", class = "invoice-button"),
        tags$hr(),
        h4("Master spreadsheet summary"),
        tableOutput("master_summary_table"),
        br(),
        h4("Extra info (Internal/External)"),
        tableOutput("extra_info_table")
      ),
      mainPanel(
        DT::dataTableOutput("data_table")
      )
    )
  )
}

generate_extra_info_table <- function(processed_data) {
    req(processed_data())
    df <- processed_data()
    if (all(c("Internal Extra", "External Extra") %in% names(df))) {
      data.frame(
        `Internal Extra` = unique(na.omit(df[["Internal Extra"]]))[1],
        `External Extra` = unique(na.omit(df[["External Extra"]]))[1]
      )
    } else {
      data.frame(`Internal Extra` = NA, `External Extra` = NA)
    }
}

generate_main_table <- function(filtered_data) {
  req(filtered_data())
  df <- filtered_data()
  desired <- c("Product Code", "Brand", "Product Category", "Product Name",
               "per reaction cost", "%PRJ surcharge", "%EXTERNAL surcharge",
               "Additional reagent Cost (not incl. in kit)")
  keep <- intersect(desired, names(df))
  validate(need(length(keep) > 0, "None of the expected columns were found. Check your master’s headers."))
  datatable(
    df[, keep, drop = FALSE],
    rownames = FALSE,
    options = list(ordering = FALSE, language = list(search = "Search Item:")),
    selection = "multiple"
  )
}